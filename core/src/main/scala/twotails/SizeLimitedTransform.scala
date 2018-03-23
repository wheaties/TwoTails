package twotails

import scala.tools.nsc.Global
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.tools.nsc.symtab.Flags._
import collection.mutable.{Map => MMap}
import collection.breakOut

//TODO: instead of replacing f: => A with f: () => A via conversion...
//      why not wrap original g = () => f. Maybe compiler removes the extra indirection?

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
  
  //New tact, take "byname" params and map them to defer(f)
  //then don't have to modify the structure of the nested functions
  final class SizeLimitedMethod(localTyper: analyzer.Typer) extends MutualMethod{

    def mkNewMethodSymbol(symbol: Symbol, name: TermName): Symbol ={
      val flags = METHOD | FINAL | PRIVATE | ARTIFACT
      val methSym = symbol.cloneSymbol(symbol.owner, flags, name)
      val param = methSym.newSyntheticValueParam(definitions.IntTpe, TermName("indx"))
    
      methSym.modifyInfo {
        case GenPolyType(tparams, MethodType(params, res)) => GenPolyType(tparams, MethodType(param :: params, res))
      }
      methSym.removeAnnotation(mtrec)
      localTyper.namer.enterInScope(methSym)
    }

    def mkNewMethodVariables(symbol: Symbol, defer: Tree): List[ValDef] ={
      val vars: List[ValDef] = symbol.info.paramss.flatten.map{ param =>
        val name = TermName(param.name + "$")
        param.tpe match{
          case TypeRef(_, definitions.ByNameParamClass, arg :: Nil) =>
            val paramSym = symbol.newVariable(name, NoPosition, ARTIFACT).setInfo{
              definitions.functionType(Nil, arg)
            }
            val rhs = localTyper.typed{
              Function(Nil, gen.mkAttributedIdent(param)) setType definitions.functionType(Nil, arg)// setType definitions.functionType(Nil, param.tpe)
            }
            ValDef(paramSym, rhs) setType definitions.UnitTpe
          case _ =>
            val paramSym = symbol.newVariable(name, NoPosition, ARTIFACT).setInfo(param.tpe)
            ValDef(paramSym, gen.mkAttributedIdent(param)) setType definitions.UnitTpe
        }
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

    private final val fn0 = definitions.FunctionClass(0)
    private def isFn0(tree: Tree): Boolean = tree match{
      case ValDef(_, _, _, rhs) => isFn0(rhs)
      case _ => tree.tpe.typeSymbol.isSubClass(fn0)
    }

    def mkVarReassignments(assigns: List[Tree], defer: Tree): List[Tree => Tree] ={

      assigns.map{ lhs =>
        { t: Tree => 
          if(isFn0(lhs) && !isFn0(t)){
            val rhs = localTyper.typed{
              Apply(defer.symbol, t) //setType definitions.functionType(Nil, t.tpe)
            }
            mkAssignVar(lhs.symbol, rhs)
          }
          else mkAssignVar(lhs.symbol, t)
        }
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
      val dn = if(isFn0(tree) && !result.tpe.typeSymbol.isSubClass(fn0)){
        mkAssignVar(result, tree) //TODO: add in Apply
      else mkAssignVar(result, tree)

      gen.mkTreeOrBlock(cnt :: dn :: Nil) setType definitions.UnitTpe
    }

    def mkDefer(methSym: Symbol): Tree ={
      val sym = methSym.newMethodSymbol(TermName("defer$"), newFlags = ARTIFACT)
      val tpe = sym.newAbstractType(TypeName("A"), newFlags = PARAM)
      tpe.setInfo(TypeBounds.empty)
      val tref = typeRef(NoPrefix, tpe, Nil)
      val byname = typeRef(NoPrefix, definitions.ByNameParamClass, tref :: Nil)
      val paramSym = sym.newValueParameter(TermName("f")) setInfo byname
      //paramSym.setFlag(BYNAMEPARAM)

      sym.setInfo(GenPolyType(List(tpe), MethodType(List(paramSym), definitions.functionType(Nil, tref))))
      val iden = gen.mkAttributedRef(paramSym)
      val rhs = Function(Nil, iden) setType definitions.functionType(Nil, tref)
      
      localTyper.typed{
        DefDef(sym, rhs)
      }
    }

    def mkNewMethodRhs(methSym: Symbol, defdef: List[Tree]): Tree ={
      val defer = mkDefer(methSym)
      val result :: continue :: index :: variables = mkNewMethodVariables(methSym, defer)
      val indexAssignments = mkIndexAssignment(index.symbol, defdef)
      val reassigns = mkVarReassignments(variables, defer)
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

        val methRhs = callTransformer.transform(rhs)
          .substituteSymbols(old, neww)
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
      System.out.println(s"CASES: $cases")

      val cntIdent = gen.mkAttributedRef(continue.symbol)
      val indxIdent = gen.mkAttributedRef(index.symbol)
      val loop = localTyper.typed{
        q"""while($cntIdent){
          ($indxIdent: @scala.annotation.switch) match{ case ..$cases }
        }"""
      }
      

      val resIdent = gen.mkAttributedRef(result.symbol)
      val block = gen.mkTreeOrBlock{
        defer :: result :: continue :: index :: variables ::: functions ::: List(loop, resIdent)
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
          p.tpe match{
            case TypeRef(_, definitions.ByNameParamClass, arg :: Nil) => 
              sym.newSyntheticValueParam(definitions.functionType(Nil, arg), TermName(p.name + ""))
            case _ => p.cloneSymbol(sym, p.flags, p.name)
          }
        }
        GenPolyType(Nil, MethodType(cloned, definitions.UnitTpe))
      }
      
      localTyper.namer.enterInScope(sym)
    }

    def forwardTrees(methSym: Symbol, defdef: List[Tree]): List[Tree] = defdef.zipWithIndex.map{
      case (tree, indx) =>
        val DefDef(_, _, _, vp :: vps, _, _) = tree
        val newVp = localTyper.typed(Literal(Constant(indx))) :: vp.map{ p => gen.paramToArg(p.symbol) }//(forwardArg)
        val refTree = gen.mkAttributedRef(tree.symbol.owner.thisType, methSym)
        val forwarderTree = (Apply(refTree, newVp) /: vps){
          (fn, params) => Apply(fn, params.map{ p => gen.paramToArg(p.symbol) })//params map forwardArg)
        }
        val forwarded = deriveDefDef(tree)(_ => localTyper.typedPos(tree.symbol.pos)(forwarderTree))
        if(tree.symbol.isEffectivelyFinalOrNotOverridden) forwarded.symbol.removeAnnotation(mtrec)
        forwarded
    }

    //by name params are never repeated params, safe to avoid that check
    /*def forwardArg(param: Tree): Tree = if(param.symbol.hasFlag(BYNAMEPARAM)){
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
    else gen.paramToArg(param.symbol)*/
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