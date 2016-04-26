package twotails

import scala.annotation.{StaticAnnotation, compileTimeOnly, switch}
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import collection.mutable.{ListBuffer, Map => MMap}
import collection.breakOut

@compileTimeOnly("Somehow this didn't get processed as part of the compilation.")
final class mutualrec extends StaticAnnotation

class TwoTailsPlugin(val global: Global) extends Plugin{
  val name = "twotails"
  val description = "Adds support for mutually recursive functions in Scala."
  val components = List[PluginComponent](new MutualRecComponent(this, global))
}

//TODO: In the future can consider mutualrec from within a def but for now only objects, traits and classes.
//TODO: must require that them mutualrec methods are "final"
class MutualRecComponent(val plugin: Plugin, val global: Global) 
    extends PluginComponent with Transform with TypingTransformers {//with TreeDSL{
  import global._

  val phaseName = "twotails"
  override val runsBefore = List("tailcalls", "patmat") //must occur before the tailcalls phase and pattern matcher
  val runsAfter = List("typer")
  override val requires = List("typer")

  def newTransformer(unit: CompilationUnit) = new MutualRecTransformer(unit)

  final class MutualRecTransformer(unit: CompilationUnit) extends TypingTransformer(unit){

    //Thanks @retronym
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val flattened = stats.flatMap {
        case Block(st, EmptyTree) => st
        case x => x :: Nil
      }
      super.transformStats(flattened, exprOwner)
    }

    override def transform(tree: Tree): Tree = super.transform{
      curTree = tree
      tree match{
        case cd @ ClassDef(mods, name, tparams, body) => treeCopy.ClassDef(cd, mods, name, tparams, transformBody(tree, body))
        case md @ ModuleDef(mods, name, body) => treeCopy.ModuleDef(md, mods, name, transformBody(tree, body))
        case _ => tree
      }
    }

    def transformBody(root: Tree, template: Template): Template ={
      val Template(parents, valDef, body) = template

      val cnt = body.count{
        case ddef: DefDef => hasMutualRec(ddef)
        case _ => false
      }

      (cnt: @switch) match{
        case 0 => template
        case 1 => convertTailRec(body); template
        case _ => treeCopy.Template(template, parents, valDef, convertMutualRec(root, body))
      }
    }

    private final val mtrec: Symbol = rootMirror.getRequiredClass("twotails.mutualrec")
    //val tred = AnnotationInfo()
    //val trec = AnnotationInfo(appliedType(TailrecClass, ???), Nil, Nil)

    def hasMutualRec(ddef: DefDef) = ddef.symbol.annotations.exists(_.tpe.typeSymbol == mtrec)

    //In case there's just one, convert to a @tailrec. No reason to add a whole new method.
    def convertTailRec(body: List[Tree]): Unit = body.foreach{
      case ddef: DefDef if hasMutualRec(ddef) => 
        ddef.symbol.removeAnnotation(mtrec)
          //.setAnnotations(trec :: Nil)
      case _ => 
    }

    def convertMutualRec(root: Tree, body: List[Tree]): List[Tree] ={
      val (head :: tail, everythingElse) = body.partition{
        case ddef: DefDef => hasMutualRec(ddef)
        case _ => false
      }

      //Right here making the assumption that every method that's mutualrec has both the same named arguments
      //with potentially the same default arguments. Will need to handle that in the future as a TODO.
      val methSym = mkNewMethodSymbol(head.symbol)
      val rhs = mkNewMethodRhs(methSym, head :: tail)
      val methTree = mkNewMethodTree(methSym, root, rhs)
      val forwardedTrees = forwardTrees(methSym, head :: tail)

      Block(everythingElse ::: (methTree :: forwardedTrees), EmptyTree) :: Nil
    }

    def mkNewMethodSymbol(symbol: Symbol, 
                          name: TermName = TermName("mutualrec_fn"), 
                          flags: FlagSet = METHOD | FINAL | PRIVATE | ARTIFACT): Symbol ={
      val methSym = symbol.cloneSymbol(symbol.owner, flags, name)
      val param = methSym.newSyntheticValueParam(definitions.IntTpe, TermName("indx"))
      
      methSym.modifyInfo {
        case GenPolyType(tparams, MethodType(params, res)) => GenPolyType(tparams, MethodType(param :: params, res))
      }
      methSym.removeAnnotation(mtrec)
      localTyper.namer.enterInScope(methSym)
    }

    def mkNewMethodRhs(methSym: Symbol, defdef: List[Tree]): Tree ={
      val defSymbols = defdef.map(_.symbol).zipWithIndex.toMap
      val callTransformer = new MutualCallTransformer(methSym, defSymbols, unit)
      val defrhs = defdef.map{ tree =>
        val DefDef(_, _, _, vparams, _, rhs) = tree

        val origTparams = tree.symbol.info.typeParams
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

        val old = oldSkolems ::: tree.symbol.typeParams ::: vparams.flatMap(_.map(_.symbol))
        val neww = deskolemized ::: methSym.typeParams ::: methSym.info.paramss.flatten.drop(1)

        val replacedRhs = callTransformer.transform(rhs)

        super.transform(replacedRhs)
          .changeOwner((tree.symbol, methSym))
          .substituteSymbols(old, neww)
      }

      val cases = defrhs.zipWithIndex.map{
        case (body, i) => cq"$i => $body"
      }

      q"(indx: @scala.annotation.switch) match{ case ..$cases }"
    }

    def mkNewMethodTree(methSym: Symbol, tree: Tree, rhs: Tree): Tree ={
      localTyper.typedPos(tree.symbol.pos)(DefDef(methSym, rhs))
    }

    //TODO: eventually must handle default args and arg lists of different arity or does it do that already?
    def forwardTrees(methSym: Symbol, defdef: List[Tree]): List[Tree] = defdef.zipWithIndex.map{
  	  case (tree, indx) =>
        val DefDef(mods, _, _, vparams @ (vp :: vps), _, _) = tree
        val newVp = localTyper.typed(Literal(Constant(indx))) :: vp.map(p => gen.paramToArg(p.symbol))
        val forwarderTree = (Apply(gen.mkAttributedRef(tree.symbol.owner.thisType, methSym), newVp) /: vps){ 
          (fn, params) => Apply(fn, params map gen.paramToArg)
        }
        val forwarded = deriveDefDef(tree)(_ => localTyper.typedPos(tree.symbol.pos)(forwarderTree))
        forwarded.symbol.removeAnnotation(mtrec)
        forwarded
    }
  }

  class MutualCallTransformer(methSym: Symbol, symbols: Map[Symbol, Int], unit: CompilationUnit) extends TypingTransformer(unit){
    override def transform(tree: Tree): Tree = tree match{
      case root: Apply if containsSym(tree) => localTyper.typedPos(tree.symbol.pos)(mkApply(root, tree))
      case _ => super.transform(tree)
    }

    //TODO: TypeApply
    def containsSym(tree: Tree): Boolean = tree match{
      case Apply(ap: Apply, _) => containsSym(ap)
      case Apply(fn, _) => symbols.contains(fn.symbol)
      case _ => symbols.contains(tree.symbol)
    }

    //TODO: TypeApply here or above in transform?
    //TODO: remove debug prints
    def mkApply(root: Tree, tree: Tree): Tree = tree match{        
      case Apply(fn: Apply, args) => System.out.println(show(args)); Apply(mkApply(root, fn), transformTrees(args))
      case Apply(fn, args) => 
        System.out.println(show(args));
        val indxParam = localTyper.typed(Literal(Constant(symbols(fn.symbol))))
        Apply(gen.mkAttributedRef(root.symbol.owner.thisType, methSym), indxParam :: transformTrees(args))
    }
  }
}