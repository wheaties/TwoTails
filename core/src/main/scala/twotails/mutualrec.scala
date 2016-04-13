package twotails

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.tools.nsc.Global
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.transform.{InfoTransform, Transform, TypingTransformers}
import collection.mutable.ListBuffer
import collection.breakOut

@compileTimeOnly("Somehow this didn't get processed as part of the compilation.")
final class mutualrec extends StaticAnnotation

//TODO: Things to get done
// 2. use class to extract all mutualrec defs
// 2a. Change @mutualrec into @tailrec if count = 1
// 3. create new @tailrec private function with additional index arg field
// 4. create @switch statement
// 5. map functions to switch statements
// 6. replace function calls in private def with prive def call and correct index
// 7. replace mutualrec defs with final def delegating to new private def
// 8. look at arg names and make sure all mapped functions share same arg names (replace, don't fail compilation)

class TwoTailsPlugin(val global: Global) extends Plugin{
  val name = "TwoTails"
  val description = "Adds support for mutually recursive functions in Scala."
  val components = List[PluginComponent](new MutualRecComponent(this, global))
}

//TODO: In the future can consider mutualrec from within a def but for now only objects, traits and classes.

class MutualRecComponent(val plugin: Plugin, val global: Global) 
    extends PluginComponent with Transform with TypingTransformers {//with TreeDSL{
  import global._

  val phaseName = "twotails"
  override val runsBefore = List("tailcalls", "patmat") //must occur before the tailcalls phase and pattern matcher
  val runsAfter = List("typer")

  def newTransformer(unit: CompilationUnit) = new MutualRecTransformer(unit)

  class MutualRecTransformer(unit: CompilationUnit) extends TypingTransformer(unit){
    override def transform(tree: Tree): Tree ={
      tree match{
        case ClassDef(mods, name, tparams, body) => ClassDef(mods, name, tparams, transformBody(body))
        case ModuleDef(mods, name, body) => ModuleDef(mods, name, transformBody(body))
        case _ => super.transform(tree)
      }
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

    def hasMutualRec(ddef: DefDef) = ddef.symbol.hasAnnotation(mtrec)

    def convertTailRec(body: List[Tree], defdef: DefDef): List[Tree] = body.map{
      case ddef @ DefDef(mods, name, tp, vp, tpt, rhs) if hasMutualRec(ddef) =>
        val newMods = mods.mapAnnotations{ x: List[Tree] =>
          x.map{
          	case q"new twotails.mutualrec" => q"new scala.annotation.tailrec" //correct for matching/making?
            case other => show(other); other
          }
        }
        DefDef(newMods, name, tp, vp, tpt, rhs)
      case item => item
    }

    def convertMutualRec(body: List[Tree], defdef: List[DefDef]): List[Tree] ={
      val name = TermName("mutualrec") //something else
      val mapping: Map[DefDef, DefDef] = defdef.zipWithIndex.map{
        case (dd, i) => (dd, defsubstitute(i, dd, name))
      }(breakOut)
      ???
    }

    //TODO: Must substitute method names in body.
    //TODO: eventually must handle default args and arg lists of different arity.
    //TODO: handle protected, private defs
    def defsubstitute(indx: Int, oldDef: DefDef, mutual: TermName, flagSet: FlagSet = FINAL): DefDef = {
  	  val DefDef(Modifiers(_, privateWithin, annotations), name, tp, vp, tpt, _) = oldDef
  	  val filteredAnnotations = annotations.filter{
  	  	case q"new twotails.mutualrec" => false //this correct for matching?
  	  	case item => show(item); true
  	  }
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
    val cases = defdef.zipWithIndex.map{
      case (DefDef(_, name, _, vp, _, body), i) =>
        val newBody = new NameReplaceTransformer(name, i, mutual).transform(body)
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

  //TODO: actually match against the name of the substituted function
  class NameReplaceTransformer(name: TermName, indx: Int, mutual: TermName) extends Transformer{
  	override def transform(tree: Tree): Tree = tree match{
  	  case q"$fn(...$args)" => q"$mutual(...${newArgs(args)})" //if name == name of fn
  	  case q"$fn[..$tp](...$args)" => q"$mutual[..$tp](...${newArgs(args)})" //if name == name of fn
  	  case _ => super.transform(tree)
  	}

  	def newArgs(args: List[List[Tree]]): List[List[Tree]] = args match{
  	  case Nil => (Literal(Constant(indx)) :: Nil) :: Nil
  	  case head :: tail => (Literal(Constant(indx)) :: head) :: tail
  	}
  }
}