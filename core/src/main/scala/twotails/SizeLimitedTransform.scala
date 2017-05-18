package twotails

import scala.tools.nsc.Global
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.tools.nsc.symtab.Flags._
import collection.mutable.{Map => MMap}
import collection.breakOut

trait SizeLimited extends Transform with TypingTransformers{
  val global: Global //Trick for the "import global._" to be recognized across files.

  import global._

  lazy val mtrec: Symbol = rootMirror.getRequiredClass("twotails.mutualrec")
  lazy val trec = AnnotationInfo(definitions.TailrecClass.tpe, Nil, Nil)

  abstract class MutualMethod extends ((Tree, List[Tree], TermName) => List[Tree]){
    def apply(root: Tree, defs: List[Tree], name: TermName): List[Tree] ={
      val head :: tail = defs
  	  val methSym = mkNewMethodSymbol(head.symbol, name)
  	  val rhs = mkNewMethodRhs(methSym, defs)
      val methTree = mkNewMethodTree(methSym, root, rhs)
      val forwarded = forwardTrees(methSym, defs)

      methTree :: forwarded
  	}

  	def mkNewMethodSymbol(symbol: Symbol, name: TermName): Symbol
  	def mkNewMethodRhs(methSym: Symbol, defdef: List[Tree]): Tree
  	def mkNewMethodTree(methodSym: Symbol, tree: Tree, rhs: Tree): Tree
    def forwardTrees(methSym: Symbol, defdef: List[Tree]): List[Tree]
  }
  
  final class SizeLimitedMethod(localTyper: analyzer.Typer) extends MutualMethod{

    def mkNewMethodSymbol(symbol: Symbol, name: TermName): Symbol ={
      val flags = METHOD | FINAL | PRIVATE | ARTIFACT
      val methSym = symbol.cloneSymbol(symbol.owner, flags, name)
      val param = methSym.newSyntheticValueParam(definitions.IntTpe, TermName("indx"))
    
      methSym.modifyInfo {
        case GenPolyType(tparams, MethodType(params, res)) => 
          val newParams = params.map{
            //preempt Uncurry phase, transform f: => A into f: () => A
            case p if p.hasFlag(BYNAMEPARAM) => 
              p.resetFlag(BYNAMEPARAM)
              p.modifyInfo{
                case GenPolyType(tp, TypeRef(_, definitions.ByNameParamClass, List(ret))) =>
                  GenPolyType(tp, definitions.functionType(Nil, ret))
              }
            case p => p
          }
          GenPolyType(tparams, MethodType(param :: newParams, res))
      }
      methSym.removeAnnotation(mtrec)
      localTyper.namer.enterInScope(methSym)
    }

    def mkNewMethodVariables(symbol: Symbol): List[ValDef] ={
      val vars: List[ValDef] = symbol.info.paramss.flatten.map{ param =>
        val name = TermName(param.name + "$")
        val paramSym = symbol.newVariable(name, NoPosition, ARTIFACT)
          .setInfo(param.tpeHK)
        ValDef(paramSym, gen.mkAttributedIdent(param)) setType definitions.UnitTpe
      }(breakOut)
      val returnVar = {
        val ret = symbol.asMethod.returnType.dealias
        val sym = symbol.newVariable(TermName("result$"), NoPosition, ARTIFACT)
          .setInfo(ret)
        //cast is required for parameters which are type params. Later phase will set 
        //type params to null(Null(null)) and error out.
        val zero = localTyper.typed{ gen.mkCast(gen.mkZero(ret), ret) }
        ValDef(sym, zero) setType definitions.UnitTpe
      }
      val continueVar = {
        val sym = symbol.newVariable(TermName("continue$"), NoPosition, ARTIFACT)
          .setInfo(definitions.BooleanTpe)
        val default = Literal(Constant(true)) setType definitions.BooleanTpe
        ValDef(sym, default) setType definitions.UnitTpe
      }

      returnVar :: continueVar :: vars
    }

    def mkVarReassignments(assigns: List[Tree]): List[Tree => Tree] = assigns.map{ 
      case lhs @ ValDef(_, _, tpt, _) => 
        new (Tree => Tree){ 
          def apply(tree: Tree) = tree match{ 
            case t: Literal => gen.mkAssign(gen.mkAttributedIdent(lhs.symbol), t) setType definitions.UnitTpe
            case t: Tree => 
              //Using tpe.widen here because of Int(0){Int(0)} vs Int{Int} types.
              val rhs = if(t.tpe.widen <:< tpt.tpe.widen) t else gen.mkAttributedRef(t.symbol)
              localTyper.typedPos(t.pos){ 
                gen.mkAssign(gen.mkAttributedIdent(lhs.symbol), rhs) 
              }
          } 
        }
    }

    def mkIndexAssignment(indx: Symbol, defdef: List[Tree]): Map[Symbol, () => Tree] =
      defdef.zipWithIndex.map{
        case (d, i) =>
          val fn = { () => 
            val rhs = Literal(Constant(i)) setType definitions.IntTpe
            gen.mkAssign(gen.mkAttributedIdent(indx), rhs) setType definitions.UnitTpe
          }
          (d.symbol, fn)
      }(breakOut)

    /** Creates the final capture of the end value to be returned from the transformed 
     *  function.
     */
    def mkDone(result: Symbol, continue: Symbol): Tree => Tree = {tree: Tree => 
      val stop = gen.mkZero(definitions.BooleanTpe)
      val cnt = gen.mkAssign(gen.mkAttributedRef(continue), stop) setType definitions.UnitTpe
      val dn = gen.mkAssign(gen.mkAttributedRef(result), tree) setType definitions.UnitTpe

      gen.mkTreeOrBlock(cnt :: dn :: Nil) setType definitions.UnitTpe
    }

    //TODO: ok, right here I should be looking at the original symbols to detect if they were
    //      byname parameters of the original method, then substitution of the tree!
    def mkNewMethodRhs(methSym: Symbol, defdef: List[Tree]): Tree ={
      val result :: continue :: index :: variables = mkNewMethodVariables(methSym)
      val indexAssignments = mkIndexAssignment(index.symbol, defdef)
      val reassigns = mkVarReassignments(variables)
      val doneFn = mkDone(result.symbol, continue.symbol)
      val callTransformer = new SizedCallTransformer(reassigns, indexAssignments, doneFn)
      val functions = defdef.map{ case tree @ DefDef(_, _, _, vparams, _, rhs) =>

        //shamelessly taken from @retronym's example #20 of the Scalac survival guide.
        val origTparams = tree.symbol.info.typeParams
        val paramSymbols = methSym.info.paramss.flatten
        val (oldSkolems, deskolemized) = if(origTparams.isEmpty) (Nil, Nil) else{
          val skolemSubst = MMap.empty[Symbol, Symbol]
          rhs.foreach{
            _.tpe.foreach {
              case tp if tp.typeSymbolDirect.isSkolem =>
                val tparam = tp.typeSymbolDirect.deSkolemize
                if (!skolemSubst.contains(tparam) && origTparams.contains(tparam)) {
                  skolemSubst(tp.typeSymbolDirect) = methSym.typeParams(origTparams.indexOf(tparam))
                }
              case _ =>
            }
          }
          skolemSubst.toList.unzip
        }

        val sym = mkNestedMethodSymbol(methSym, tree)
        val vpsyms = vparams.flatMap(_.map(_.symbol))
        val old = oldSkolems ::: tree.symbol.typeParams ::: vpsyms
        val neww = deskolemized ::: methSym.typeParams ::: sym.info.paramss.flatten

        val byNamePairs = vpsyms.zip(sym.info.paramss.flatten).filter{
          case (orig, _) => orig.hasFlag(BYNAMEPARAM)
        }.toMap
        val byNameTransforer = new ByNameTransformer(byNamePairs)

        val methRhs = callTransformer.transform{
          byNameTransforer.transform(rhs)
            .substituteSymbols(old, neww)
            .changeOwner(tree.symbol -> sym)
          
        }

        localTyper.typed{
          DefDef(sym, methRhs)
        }
      }

      val cases = functions.zipWithIndex.map{
        case (nest, i) => 
          val syms = variables.map{ x => gen.mkAttributedRef(x.symbol) }
          cq"$i => ${nest.symbol}(..$syms)"
      }

      val cntIdent = gen.mkAttributedRef(continue.symbol)
      val indxIdent = gen.mkAttributedRef(index.symbol)
      val loop = localTyper.typed{
        q"""while ($cntIdent){
          ($indxIdent: @scala.annotation.switch) match{ case ..$cases }
        }"""
      }

      val resIdent = gen.mkAttributedRef(result.symbol)
      val block = gen.mkTreeOrBlock{
        result :: continue :: index :: variables ::: functions ::: List(loop, resIdent)
      }
      localTyper.typed{ block }
    }

    def mkNewMethodTree(methodSym: Symbol, tree: Tree, rhs: Tree): Tree =
      localTyper.typedPos(tree.pos){
        DefDef(methodSym, rhs)
      }

    def mkNestedMethodSymbol(methSym: Symbol, orig: DefDef) ={
      val name = TermName(orig.name + "$")
      val sym = methSym.newMethod(name, NoPosition, ARTIFACT)
      sym.setInfo {
        //Reusing methSym.info allows us to avoid having to redo BYNAMEPARAM modification.
        val cloned = methSym.info.paramss.flatten.drop(1).map{ p =>
          p.cloneSymbol(sym, p.flags, p.name) 
        }
        GenPolyType(Nil, MethodType(cloned, definitions.UnitTpe))
      }
      
      localTyper.namer.enterInScope(sym)
    }

    def forwardTrees(methSym: Symbol, defdef: List[Tree]): List[Tree] = defdef.zipWithIndex.map{
      case (tree, indx) =>
        val DefDef(_, _, _, vp :: vps, _, _) = tree
        val newVp = localTyper.typed(Literal(Constant(indx))) :: vp.map(forwardArg)
        val refTree = gen.mkAttributedRef(tree.symbol.owner.thisType, methSym)
        val forwarderTree = (Apply(refTree, newVp) /: vps){
          (fn, params) => Apply(fn, params map forwardArg)
        }
        val forwarded = deriveDefDef(tree)(_ => localTyper.typedPos(tree.symbol.pos)(forwarderTree))
        if(tree.symbol.isEffectivelyFinalOrNotOverridden) forwarded.symbol.removeAnnotation(mtrec)
        forwarded
    }

    def forwardArg(param: Tree): Tree ={
      //by name params are never repeated params, safe to avoid that check
      if(param.symbol.hasFlag(BYNAMEPARAM)){
        val ref = gen.mkAttributedRef(param.symbol)
        localTyper.typedPos(param.pos){ Function(Nil, ref) }
      }
      else gen.paramToArg(param.symbol)
    }
  }

  /** Remaps the byname parameters into an APPLY much like what will happen during UnCurry.
   *  TODO: Figure out a way to move after "uncurry." Would save a lot of hassle.
   */
  final class ByNameTransformer(sub: Map[Symbol, Symbol]) extends Transformer{
    override def transform(tree: Tree): Tree = tree match{
      //take {() => byname} and convert to byname
      case Function(Nil, x) if sub.contains(x.symbol) => gen.mkAttributedRef(sub(x.symbol))
      case Ident(_) if sub.contains(tree.symbol) => remapByName(tree)
      case _ => super.transform(tree)
    }

    //assumes tree.tpe != gen.mkAttributedRef(symbol).tpe
    private def remapByName(tree: Tree): Tree ={
      val ref = gen.mkAttributedRef(sub(tree.symbol))
      atPos(tree.pos){
        Apply(ref, Nil) setType tree.tpe
      }
    }
  }

  /** @note  The newly created method symbol can be reused without issue but if the symbol `Tree`
   *         of the index literals are reused, creates a `NullPointerException` during Erasure 
   *         phase.
   *         Also, this does not handle the "and" or "or" operators (&&, || respectively) even
   *         though `scala.annotation.tailcall` can.
   *
   *  @param state The set of transforms from arguments into shared variable reassignments
   *  @param index the lookup table for index reassignment
   *  @param done converts the return value into shared variable reassignment
   */
  final class SizedCallTransformer(state: List[Tree => Tree],
                                   index: Map[Symbol, () => Tree],
                                   done: Tree => Tree) extends Transformer{

    override def transform(tree: Tree): Tree = tree match{
      case Apply(fn, _) if index.contains(fn.symbol) =>
        gen.mkTreeOrBlock(multiArgs(tree)) setType definitions.UnitTpe
      case Block(stats, expr) =>
        val exp = transform(expr)
        treeCopy.Block(tree, stats, exp) setType exp.tpe
      case If(pred, t, f) =>
        val ttrue = transform(t)
        val ffalse = transform(f)
        treeCopy.If(tree, pred, ttrue, ffalse) setType ifType(ttrue.tpe, ffalse.tpe, tree.tpe)
      case Match(selector, cases) => treeCopy.Match(tree, selector, transformCaseDefs(cases))
      case Try(block, catches, EmptyTree) => 
        treeCopy.Try(tree, block, transformCaseDefs(catches), EmptyTree)
      case that: Try => that
      case that: Apply => done(that)
      case that: Select => done(that)
      case that: Ident => done(that)
      case that: Literal => done(that)
      case that: ValDef => that
      case CaseDef(pat, guard, body) => treeCopy.CaseDef(tree, pat, guard, transform(body))
      case that: AppliedTypeTree => that
      case that: Bind => that
      case that: Function => done(that)
      case that: New => done(that)
      case that: Assign => done(that)
      case that: AssignOrNamedArg => done(that)
      case that: Throw => that //well, it ain't 'done' if it's going to throw.
      case that: Super => done(that)
      case that: TypeBoundsTree => that
      case that: Typed => that
      case that: Import => that
      case that: Template => that
      case that: TypeApply => that
      case that: DefDef => that
      case that: ClassDef => that
      case that: ModuleDef => that
      case that: TypeDef => that
      case that: LabelDef => that
      case that: PackageDef => that
      case that: Annotated => that
      case that: SingletonTypeTree => that
      case that: SelectFromTypeTree => that
      case that: CompoundTypeTree => that
      case that: ExistentialTypeTree => that
      case Return(expr) => done(expr) //Not going to try to recreate all the problems with return.
      case that: Alternative => that
      case that: Star => that
      case that: UnApply => that
      case that: ArrayValue => that
      case that: ApplyDynamic => that
      case that: ReferenceToBoxed => that //TODO: should this be "done?"
      case _ => super.transform(tree) 
    }

    def ifType(ttrue: Type, ffalse: Type, res: Type): Type = (ttrue, ffalse) match{
      case (definitions.UnitTpe, _) => definitions.UnitTpe
      case (_, definitions.UnitTpe) => definitions.UnitTpe
      case _ => res
    }

    def multiArgs(tree: Tree, acc: List[List[Tree]] = Nil): List[Tree] = tree match{
      case Apply(fn, args) => multiArgs(fn, args :: acc)
      case _ => 
        index(tree.symbol)() :: acc.flatten.zip(state).map{
          case (arg, fn) => fn(arg) 
        }
    }
  }
}