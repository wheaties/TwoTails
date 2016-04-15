package twotails

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.transform.{InfoTransform, Transform, TypingTransformers}
import collection.mutable.ListBuffer
import collection.breakOut

@compileTimeOnly("Somehow this didn't get processed as part of the compilation.")
final class mutualrec extends StaticAnnotation

class TwoTailsPlugin(val global: Global) extends Plugin{
  val name = "twotails"
  val description = "Adds support for mutually recursive functions in Scala."
  val components = List[PluginComponent](new MutualRecComponent(this, global))
}

//TODO: In the future can consider mutualrec from within a def but for now only objects, traits and classes.
//TODO: Does this actually add a new phase? Still get warning message... "Transform" should do that...
class MutualRecComponent(val plugin: Plugin, val global: Global) 
    extends PluginComponent with Transform with TypingTransformers {//with TreeDSL{
  import global._

  val phaseName = "twotails"
  override val runsBefore = List("tailcalls", "patmat") //must occur before the tailcalls phase and pattern matcher
  val runsAfter = List("typer")
  override val requires = List("typer")

  def newTransformer(unit: CompilationUnit) = new MutualRecTransformer(unit)

  //TODO: This only handles top level class, trait and object. Does not handle nested structures.
  class MutualRecTransformer(unit: CompilationUnit) extends TypingTransformer(unit){
    override def transform(tree: Tree): Tree = tree match{
      case ClassDef(mods, name, tparams, body) => ClassDef(mods, name, tparams, transformBody(body))
      case ModuleDef(mods, name, body) => ModuleDef(mods, name, transformBody(body))
      case _ => super.transform(tree)
    }

    def transformBody(template: Template): Template ={
      val Template(parents, valDef, body) = template

      val recs = ListBuffer.empty[DefDef]
      body.foreach{
        case ddef: DefDef if hasMutualRec(ddef) => recs += ddef
        case _ =>
      }

      recs.result() match{
        case Nil => template
        case head :: Nil => Template(parents, valDef, convertTailRec(body, head))
        case defs => Template(parents, valDef, convertMutualRec(body, defs))
      }
    }

    val mtrec: Symbol = rootMirror.getRequiredClass("twotails.mutualrec")

    def hasMutualRec(ddef: DefDef) = ddef.symbol.annotations.exists(_.tpe.typeSymbol == mtrec)

    def convertTailRec(body: List[Tree], defdef: DefDef): List[Tree] = body.map{
      case ddef @ DefDef(mods, name, tp, vp, tpt, rhs) if hasMutualRec(ddef) =>
        val newMods = mods.mapAnnotations{
          _.map{ anno => if(anno.symbol == mtrec) q"new scala.annotation.tailrec" else anno }
        }
        DefDef(newMods, name, tp, vp, tpt, transform(rhs))
      case x: ClassDef => transform(x)
      case x: ModuleDef => transform(x)
      case item => item
    }

    def convertMutualRec(body: List[Tree], defdef: List[DefDef]): List[Tree] ={
      val name = TermName("mutualrec_fn") //something else, lest we get shadowing
      val mapping: Map[DefDef, DefDef] = defdef.zipWithIndex.map{
        case (dd, i) => (dd, defsubstitute(i, dd, name))
      }(breakOut)
      
      body.map{
        case x: DefDef => mapping.getOrElse(x, x)
        case x: ClassDef => transform(x)
        case x: ModuleDef => transform(x)
        case other => other
      }
    }

    //TODO: eventually must handle default args and arg lists of different arity.
    //TODO: handle protected, private defs
    def defsubstitute(indx: Int, oldDef: DefDef, mutual: TermName, flagSet: FlagSet = FINAL): DefDef = {
  	  val DefDef(Modifiers(_, privateWithin, annotations), name, tp, vp, tpt, _) = oldDef
  	  val filteredAnnotations = annotations.filter{ _.symbol != mtrec }
  	  val mods = Modifiers(flagSet, privateWithin, filteredAnnotations)
  	  val mappedArgs = vp map{
  	  	_.map{ x => Ident(x.name) }
  	  }
  	  val args = mappedArgs match{
  	  	case head :: tail => (Literal(Constant(indx)) :: head) :: tail
  	  	case Nil => (Literal(Constant(indx)) :: Nil) :: Nil
  	  }

  	  q"$mods def $name[..$tp](...$vp): $tpt ={ $mutual(...$args) }"
    }
  }

  def mutualSubstitute(mutual: TermName, defdef: List[DefDef]): DefDef ={
  	val indexed = defdef.zipWithIndex
    val symbols: Map[Symbol, Int] = indexed.map{
      case (x: DefDef, i) => (x.symbol, i)
    }(breakOut)
    val cases = indexed.map{
      case (DefDef(_, name, _, vp, _, body), i) =>
        val newBody = new NameReplaceTransformer(symbols, mutual).transform(body)
        cq"$i => $newBody"
    }
    val (tparams, vparams, tpt) = defdef match{
      case DefDef(_, _, tp, vp, t, _) :: tail => 
        val vps = vp match{
          case Nil => (q"indx: Int" :: Nil) :: Nil
          case head :: tail => (q"indx: Int" :: head) :: tail
        }

        (tp, vps, t)
    }
 
    q"@scala.annotation.tailrec private final def $mutual[..$tparams](...$vparams): $tpt ={ (indx: @scala.annotation.switch) match{ case ..$cases } }"
  }

  class NameReplaceTransformer(symbols: Map[Symbol, Int], mutual: TermName) extends Transformer{
  	override def transform(tree: Tree): Tree = tree match{
  	  case q"$fn(...$args)" if symbols.contains(fn.symbol) => q"$mutual(...${newArgs(fn, args)})"
  	  case q"$fn[..$tp](...$args)" if symbols.contains(fn.symbol) => q"$mutual[..$tp](...${newArgs(fn, args)})"
  	  case _ => super.transform(tree)
  	}

  	def newArgs(fn: Tree, args: List[List[Tree]]): List[List[Tree]] ={
  	  val indx: Int = symbols(fn.symbol)
  	  args match{
  	    case Nil => (Literal(Constant(indx)) :: Nil) :: Nil
  	    case head :: tail => (Literal(Constant(indx)) :: head) :: tail
  	  }
  	}
  }
}