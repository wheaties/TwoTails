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
        case defs => Template(parents, valDef, convertMutualRec(template.symbol.owner, body, defs))
      }
    }

    val mtrec: Symbol = rootMirror.getRequiredClass("twotails.mutualrec")
    //val tred = AnnotationInfo()
    //val trec = AnnotationInfo(appliedType(TailrecClass, ???), Nil, Nil)

    def hasMutualRec(ddef: DefDef) = ddef.symbol.annotations.exists(_.tpe.typeSymbol == mtrec)

    def convertTailRec(body: List[Tree], defdef: DefDef): List[Tree] = body.map{
      case ddef @ DefDef(mods, name, tp, vp, tpt, rhs) if hasMutualRec(ddef) =>
        val newDef = treeCopy.DefDef(ddef, mods, name, tp, vp, tpt, rhs)

        //TODO: This is just to figure out how to make a scala.annotation.tailrec
        ddef.symbol.annotations match{
          case AnnotationInfo(l, r, t) :: _ => 
            System.out.println(l)
            System.out.println(r)
            System.out.println(t)

          case _ => ()
        }
        newDef.symbol.removeAnnotation(mtrec)
          //.setAnnotations(trec :: Nil)
        newDef
      case item => item
    }

    def convertMutualRec(owner: Symbol, body: List[Tree], defdef: List[DefDef]): List[Tree] ={
      val name = TermName("mutualrec_fn") //something else, lest we get shadowing
      val mapping: Map[DefDef, DefDef] = defdef.zipWithIndex.map{
        case (dd, i) => (dd, defsubstitute(i, dd, name))
      }(breakOut)
      
      val newBody = body.map{
        case x: DefDef => mapping.getOrElse(x, transform(x))
        case other => transform(other)
      }

      newBody ::: List(mutualSubstitute(owner, name, defdef))
    }

    //TODO: eventually must handle default args and arg lists of different arity.
    def defsubstitute(indx: Int, oldDef: DefDef, mutual: TermName, flagSet: FlagSet = FINAL): DefDef = {
  	  val DefDef(mods, name, tp, vp, tpt, _) = oldDef
  	  val mappedArgs = vp map{
  	  	_.map{ x => Ident(x.name) }
  	  }
  	  val args = mappedArgs match{
  	  	case head :: tail => (Literal(Constant(indx)) :: head) :: tail
  	  	case Nil => (Literal(Constant(indx)) :: Nil) :: Nil
  	  }

      val newDef = treeCopy.DefDef(oldDef, mods, name, tp, vp, tpt, q"$mutual(...$args)")
      newDef.symbol.removeAnnotation(mtrec)
      newDef
    }

    def mutualSubstitute(owner: Symbol, mutual: TermName, defdef: List[DefDef]): Tree ={
      val indexed = defdef.zipWithIndex
      val symbols: Map[Symbol, Int] = indexed.map{
        case (x: DefDef, i) => (x.symbol, i)
      }(breakOut)

      val cases = indexed.map{
        case (DefDef(_, name, _, vp, _, body), i) =>
          val newBody = new NameReplaceTransformer(symbols, mutual).transform(body)
          cq"$i => $newBody"
      }
      val body = q"(indx: @scala.annotation.switch) match{ case ..$cases }"

      val DefDef(_, _, tp, vp, _, _) = defdef.head
      val vps = vp match{
        case Nil => (q"val indx: Int" :: Nil) :: Nil
        case head :: tail => (q"val indx: Int" :: head) :: tail
      }

      val meth = mkNewMethod(owner, defdef.head, mutual)
      val DefDef(mods, name, _, _, tpt, _) = meth
      val newDef = treeCopy.DefDef(meth, mods, name, tp, vps, tpt, body)
      localTyper.typed(newDef)
    }

    def mkNewMethod(owner: Symbol, base: DefDef, name: TermName, flags: FlagSet = FINAL | PRIVATE): DefDef ={
      val DefDef(_, _, _, _, _, body) = base
      val methSym = owner.newMethod(name, NoPosition, flags)
      //TODO: add in additional parameters (that's the Nil portion)
      methSym setInfo MethodType(Nil, base.symbol.tpe.resultType)
      methSym.owner = base.symbol.owner
      DefDef(methSym, body)
    }
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