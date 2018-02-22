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

      //handle multi-parameter arg lists
      def modRes(typ: Type): Type = typ match{
        case MethodType(params, res) => 
          MethodType(replaceByNameParams(methSym, params), modRes(res))
        case _ => typ
      }
    
      methSym.modifyInfo {
        case GenPolyType(tparams, MethodType(params, res)) => 
          val newParams = replaceByNameParams(methSym, params)
          val newRes = modRes(res)
          GenPolyType(tparams, MethodType(param :: newParams, newRes))
        case GenPolyType(tparams, NullaryMethodType(res)) => //Why do they do this?!
          GenPolyType(tparams, MethodType(param :: Nil, res))
      }
      methSym.removeAnnotation(mtrec)
      localTyper.namer.enterInScope(methSym)
    }

    def replaceByNameParams(methSym: Symbol, params: List[Symbol]): List[Symbol] =
      params.map{
        //preempt Uncurry phase, transform f: => A into f: () => A
        case p if p.hasFlag(BYNAMEPARAM) => 
          val GenPolyType(_, TypeRef(_, _, List(ret))) = p.info
          methSym.newSyntheticValueParam(definitions.functionType(Nil, ret), p.name.toTermName)
        case p => p
      }

    def mkNewMethodVariables(symbol: Symbol): List[ValDef] ={
      val vars: List[ValDef] = symbol.info.paramss.flatten.map{ param =>
        val name = TermName(param.name + "$")
        val paramSym = symbol.newVariable(name, NoPosition, ARTIFACT)
          .setInfo(param.tpe)
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

    private def mkAssignVar(t: Tree, r: Tree): Tree = mkAssignVar(t.symbol, r)
    private def mkAssignVar(symbol: Symbol, r: Tree): Tree =
      gen.mkAssign(gen.mkAttributedRef(symbol), r) setType definitions.UnitTpe

    //TODO: Somehow get this phase after Uncurry. This doesn't capture corner cases in
    //      byname parameters...
    //TODO: THE BUG MANIFESTS HERE! Doesn't mean this is where it's at...
    def mkVarReassignments(assigns: List[Tree]): List[(Tree, Symbol) => Tree] ={
      val fn0 = definitions.FunctionClass(0)
      def isFn0(tree: Tree): Boolean = tree match{
        case ValDef(_, _, _, rhs) => isFn0(rhs)
        case _ => tree.tpe.typeSymbol.isSubClass(fn0)
      }

      def replace(lhs: Tree, rhs: Tree, currentOwner: Symbol): Tree ={
        val newRhs = rhs match{
          case _ if isFn0(rhs) => rhs
          case Apply(fn, Nil) if isFn0(fn) => fn
          case _ =>
            val fn = localTyper.typedPos(rhs.pos){
              Function(Nil, rhs)
            }
            //this doesn't actually fix the problem...
            fn.symbol.modifyInfo{
              _ => rhs.tpe.dealias match{
                //TODO: should I widen here?
                case tp @ ConstantType(_) => definitions.functionType(Nil, tp.widen)
                case tp: TypeRef => definitions.functionType(Nil, tp)
              }
            }
            fn.asInstanceOf[Function].body changeOwner (currentOwner -> fn.symbol)
            fn
        }
        mkAssignVar(lhs, newRhs)
      }

      //attempting to handle blocks of code that should become anonymous functions.
      assigns.map{ lhs =>
        if(!isFn0(lhs)) { (t: Tree, s: Symbol) => mkAssignVar(lhs, t) }
        else { (t: Tree, s: Symbol) => replace(lhs, t, s) }
      }
    }

    def mkIndexAssignment(indx: Symbol, defdef: List[Tree]): Map[Symbol, () => Tree] =
      defdef.zipWithIndex.map{
        case (d, i) =>
          val fn = { () => 
            val rhs = Literal(Constant(i)) setType definitions.IntTpe
            mkAssignVar(indx, rhs)
          }
          (d.symbol, fn)
      }(breakOut)

    /** Creates the final capture of the end value to be returned from the transformed 
     *  function.
     */
    def mkDone(result: Symbol, continue: Symbol): Tree => Tree = {tree: Tree => 
      val stop = gen.mkZero(definitions.BooleanTpe)
      val cnt = mkAssignVar(continue, stop)
      val dn = mkAssignVar(result, tree)

      gen.mkTreeOrBlock(cnt :: dn :: Nil) setType definitions.UnitTpe
    }

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
        }
        methRhs.substituteSymbols(old, neww)
          .changeOwner(tree.symbol -> sym)

        localTyper.typed{
          DefDef(sym, methRhs)
        }
      }

      val cases = functions.zipWithIndex.map{
        case (nest, i) => 
          val nestRef = gen.mkAttributedRef(nest.symbol)
          val syms = variables.map{ x => gen.mkAttributedRef(x.symbol) }
          cq"$i => $nestRef(..$syms)"
      }

      val cntIdent = gen.mkAttributedRef(continue.symbol)
      val indxIdent = gen.mkAttributedRef(index.symbol)
      val loop = localTyper.typed{
        q"""while($cntIdent){
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

    //by name params are never repeated params, safe to avoid that check
    def forwardArg(param: Tree): Tree = if(param.symbol.hasFlag(BYNAMEPARAM)){
      val ref = gen.mkAttributedRef(param.symbol)
      val arg = localTyper.typedPos(param.pos){ Function(Nil, ref) }
      arg.symbol.modifyInfo{
        _ => definitions.functionType(Nil, param.tpe)
        //_ => param.symbol.tpe match{
        //  case TypeRef(_, _, List(arg)) => definitions.functionType(Nil, arg)
        //}
      }
      arg
    }
    else gen.paramToArg(param.symbol)
  }

  /** Remaps the byname parameters into an APPLY much like what will happen during UnCurry.
   */
  final class ByNameTransformer(sub: Map[Symbol, Symbol]) extends Transformer{
    override def transform(tree: Tree): Tree = tree match{
      //take {() => byname} and convert to just byname (it's already a Function0)
      case Function(Nil, x) if sub.contains(x.symbol) => atPos(tree.pos){
        gen.mkAttributedRef(sub(x.symbol))
      }
      case Ident(_) if sub.contains(tree.symbol) => remapByName(tree)
      case _ => super.transform(tree)
    }

    //TODO: issue doesn't appear in here
    private def remapByName(tree: Tree): Tree ={
      val ref = gen.mkAttributedRef(sub(tree.symbol))
      atPos(tree.pos){
        //Apply(Select(ref, nme.apply), Nil) setType tree.tpe
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
  final class SizedCallTransformer(state: List[(Tree, Symbol) => Tree],
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
          case (arg, fn) => fn(arg, currentOwner) 
        }
    }
  }
}