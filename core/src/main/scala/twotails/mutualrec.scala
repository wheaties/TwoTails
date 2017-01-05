package twotails

import scala.annotation.{switch, tailrec}
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import collection.mutable.{Map => MMap, Set => MSet}
import collection.breakOut

class TwoTailsPlugin(val global: Global) extends Plugin{
  val name = "twotails"
  val description = "Adds support for mutually recursive functions in Scala."

  override def init(options: List[String], error: String => Unit): Boolean ={
    //TODO: add any options and filter them out before passing to `super.init`
    super.init(options, error)
  }

  //see: https://github.com/scala/scala/blob/2.12.x/src/compiler/scala/tools/nsc/plugins/Plugin.scala
  override val optionsHelp: Option[String] = None 

  val components = List[PluginComponent](new MutualRecComponent(global))
}

final class MutualRecComponent(val global: Global) 
    extends PluginComponent with Transform with TypingTransformers {
  import global._

  val phaseName = "twotails"
  override val runsBefore = List("tailcalls", "uncurry")
  val runsAfter = List("typer", "patmat")

  def newTransformer(unit: CompilationUnit) = new MutualRecTransformer(unit)

  final class MutualRecTransformer(unit: CompilationUnit) extends TypingTransformer(unit){

    override def transform(tree: Tree): Tree = super.transform{
      tree match{
        case cd @ ClassDef(mods, name, tparams, body) =>
          val trans = transformBody(tree, body)
          if(trans != body) treeCopy.ClassDef(cd, mods, name, tparams, trans) else cd
        case md @ ModuleDef(mods, name, body) =>
          val trans = transformBody(tree, body)
          if(trans != body) treeCopy.ModuleDef(md, mods, name, trans) else md
        case bl @ Block(stats, expr) if expr != EmptyTree =>
          val trans = transformNested(tree, stats)
          if(stats != trans) treeCopy.Block(bl, trans, expr) else bl
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

    def transformNested(root: Tree, stats: List[Tree]): List[Tree] ={
      val cnt = stats.count{
        case ddef: DefDef => hasMutualRec(ddef)
        case _ => false
      }

      (cnt: @switch) match{
        case 0 => stats
        case 1 => convertTailRec(stats); stats
        case _ => convertMutualRec(root, stats)
      }
    }

    private final val mtrec: Symbol = rootMirror.getRequiredClass("twotails.mutualrec")
    private final val trec = AnnotationInfo(definitions.TailrecClass.tpe, Nil, Nil)

    def hasMutualRec(tree: Tree) = 
      (tree.symbol != NoSymbol) &&
      tree.symbol.annotations.exists(_.tpe.typeSymbol == mtrec)

    //In case there's just one, convert to a @tailrec. No reason to add a whole new method.
    def convertTailRec(body: List[Tree]): Unit = body.foreach{
      case ddef: DefDef if hasMutualRec(ddef) => 
        ddef.symbol
          .removeAnnotation(mtrec)
          .setAnnotations(trec :: Nil)
      case _ => 
    }

    def convertMutualRec(root: Tree, body: List[Tree]): List[Tree] ={
      val (recs, everythingElse) = body.partition{
        case ddef: DefDef => hasMutualRec(ddef)
        case _ => false
      }
      val symbols: Map[Symbol, Tree] = recs.map{ t => (t.symbol, t) }(breakOut)
      val walker = new CallGraphWalker(symbols.keySet)
      val adjacencyList: Map[Symbol, List[Symbol]] = recs.map{ tree =>
        val calls = walker.walk(tree)
        (tree.symbol, calls)
      }(breakOut)
      val groups ={
        var grouped: List[List[Symbol]] = Nil
        for(t <- recs if !grouped.exists(_.contains(t.symbol))){
          grouped = component(t.symbol, adjacencyList) :: grouped
        }
        grouped.map(_.map(symbols))
      }
      val ungrouped = recs.filter{ t => !groups.exists(_.contains(t)) }

      var tndx = 0
      val optimized = groups flatMap {
        case Nil => Nil
        case head :: Nil => head.symbol
          .removeAnnotation(mtrec)
          .setAnnotations(trec :: Nil)
          head :: Nil
        case head :: tail => 
          val methSym = mkNewMethodSymbol(head.symbol, TermName("mutualrec_fn$" + tndx))
          val rhs = mkNewMethodRhs(methSym, head :: tail)
          val methTree = mkNewMethodTree(methSym, root, rhs)
          val forwardedTrees = forwardTrees(methSym, head :: tail)
          tndx += 1
          methTree :: forwardedTrees
      }

      if(ungrouped.isEmpty) everythingElse ::: optimized
      else everythingElse ::: ungrouped ::: optimized
    }

    def component(root: Symbol, adjacencyList: Map[Symbol, List[Symbol]]): List[Symbol] ={
      //checking that the path is one that should be followed
      def hasSame(that: Symbol): Boolean = that.info.matches(root.info)

      val out = MSet.empty[Symbol]
      val visited = MSet.empty[Symbol]
      def visit(s: Symbol, path: List[Symbol] = Nil): Unit = {
        visited += s
        adjacencyList(s) foreach{
          case `root` => out += s ++= path
          case c if out.contains(c) => out ++= path
          case c if visited.contains(c) => //do nothing
          case c if hasSame(c) => visit(c, s :: path)
          case c => visited += c
        }
      }
      visit(root)
      out.filter(adjacencyList(_).nonEmpty).toList
    }

    //TODO: Make a by-name parameter capture class.
    //
    //class Capture(val x$1: => A, val x$2: => A...)
    //var capture$: Capture = Capture(var1, var2...)
    //then within objects, will have to filter out the args that do this.
    //
    //def one$():Unit ={
    //  continue$ = true
    //  capture$ = Capture(
    //    foo,
    //    bar = stuff())
    //}

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

    //TODO: Handle foo: => Any, i.e. a Thunk
    def mkNewMethodVariables(symbol: Symbol): List[Tree] ={
      val vars: List[Tree] = symbol.info.paramss.flatten.map{ param =>
        val name = TermName(param.name + "$")
        val paramSym = symbol.newVariable(name, NoPosition, ARTIFACT)
          .setInfo(param.tpeHK)
        ValDef(paramSym, gen.mkAttributedIdent(param))
      }(breakOut)
      val returnVar = {
        val ret = symbol.asMethod.returnType.dealias
        val sym = symbol.newVariable(TermName("result$"), NoPosition, ARTIFACT)
          .setInfo(ret)
        //cast is required for parameters which are type params. Later phase will set 
        //type params to null(Null(null)) and error out.
        val zero = gen.mkCast(gen.mkZero(ret), ret)
        ValDef(sym, zero)
      }
      val continueVar = {
        val sym = symbol.newVariable(TermName("continue$"), NoPosition, ARTIFACT)
          .setInfo(definitions.BooleanTpe)
        val default = Literal(Constant(true)) setType definitions.BooleanTpe
        ValDef(sym, default)
      }

      returnVar :: continueVar :: vars
    }

    def mkVarReassignments(assigns: List[Tree]): List[Tree => Tree] = assigns.map{ lhs => 
      {t: Tree => localTyper.typedPos(t.pos){ gen.mkAssign(gen.mkAttributedIdent(lhs.symbol), t) } }
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

    def mkDone(result: Symbol, continue: Symbol): Tree => Tree = {tree: Tree => 
      val stop = gen.mkZero(definitions.BooleanTpe)
      val cnt = gen.mkAssign(gen.mkAttributedRef(continue), stop) setType definitions.UnitTpe
      val dn = gen.mkAssign(gen.mkAttributedRef(result), tree) setType definitions.UnitTpe

      gen.mkTreeOrBlock(cnt :: dn :: Nil) setType definitions.UnitTpe
    }

    def mkNewMethodRhs(methSym: Symbol, defdef: List[Tree]): Tree ={
      val result :: continue :: index :: variables = mkNewMethodVariables(methSym)
      val indexAssignments = mkIndexAssignment(index.symbol, defdef)
      val reassigns = mkVarReassignments(variables)
      val doneFn = mkDone(result.symbol, continue.symbol)
      val callTransformer = new MutualCallTransformer(reassigns, indexAssignments, doneFn)
      val functions = defdef.map{ case tree @ DefDef(_, _, _, vparams, _, rhs) =>

        //shamelessly taken from @retronym's example #20 of the Scalac survival guide.
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
        val neww = deskolemized ::: methSym.typeParams ::: variables.map(_.symbol)

        val methRhs = callTransformer.transform{
          rhs.substituteSymbols(old, neww)
        }

        mkNestedMethodTree(methSym, tree, methRhs)
      }

      val cases = functions.zipWithIndex.map{
        case (nest, i) => cq"$i => ${nest.symbol}()"
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
      block setType methSym.asMethod.returnType
    }

    def mkNewMethodTree(methodSym: Symbol, tree: Tree, rhs: Tree): Tree =
      localTyper.typedPos(tree.pos){
        DefDef(methodSym, rhs)
      }

    def forwardTrees(methSym: Symbol, defdef: List[Tree]): List[Tree] = defdef.zipWithIndex.map{
  	  case (tree, indx) =>
        val DefDef(_, _, _, vp :: vps, _, _) = tree
        val newVp = localTyper.typed(Literal(Constant(indx))) :: vp.map(p => gen.paramToArg(p.symbol))
        val refTree = gen.mkAttributedRef(tree.symbol.owner.thisType, methSym)
        val forwarderTree = (Apply(refTree, newVp) /: vps){
          (fn, params) => Apply(fn, params map (p => gen.paramToArg(p.symbol)))
        }
        val forwarded = deriveDefDef(tree)(_ => localTyper.typedPos(tree.symbol.pos)(forwarderTree))
        if(tree.symbol.isEffectivelyFinalOrNotOverridden) forwarded.symbol.removeAnnotation(mtrec)
        forwarded
    }

    def mkNestedMethodTree(methSym: Symbol, orig: DefDef, rhs: Tree): Tree ={
      val name = TermName(orig.name + "$")
      val sym = methSym.newMethod(name, NoPosition, ARTIFACT)
      sym.setInfo {
        GenPolyType(Nil, MethodType(Nil, definitions.UnitTpe))
      }
      localTyper.namer.enterInScope(sym)

      localTyper.typed{
        DefDef(sym, rhs.changeOwner(orig.symbol -> sym))
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
  final class MutualCallTransformer(state: List[Tree => Tree],
                                    index: Map[Symbol, () => Tree],
                                    done: Tree => Tree) extends Transformer{

    override def transform(tree: Tree): Tree = tree match{
      case Apply(fn, _) if index.contains(fn.symbol) =>
        gen.mkTreeOrBlock(multiArgs(tree)) setType definitions.UnitTpe
      case Block(stats, expr) =>
        val exp = transform(expr)
        treeCopy.Block(tree, transformStats(stats, currentOwner), exp) setType exp.tpe
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
      case (definitions.UnitTpe, definitions.UnitTpe) => definitions.UnitTpe
      case (definitions.UnitTpe, _) => definitions.UnitTpe
      case (_, definitions.UnitTpe) => definitions.UnitTpe
      case _ => res
    }

    //TODO: How to handle a thunk?
    def multiArgs(tree: Tree, acc: List[List[Tree]] = Nil): List[Tree] = tree match{
      case Apply(fn, args) => multiArgs(fn, args :: acc)
      case _ => 
        index(tree.symbol)() :: acc.flatten.zip(state).map{
          case (arg, fn) => fn(arg) 
        }
    }
  }

  //Searches the Tree and returns all functions found in tailcall position from a search set.
  final class CallGraphWalker(search: Set[Symbol]) extends Traverser{
    private final val calls = MSet.empty[Symbol]
    private var isValid = true

    def walk(tree: Tree): List[Symbol] ={
      calls clear ()
      tree match { 
        case x:DefDef => traverse(x.rhs)
        case _ => ()
      }
      calls.toList
    }

    def step(tree: Tree, valid: Boolean): Unit ={
      val oldValid = isValid
      isValid &= valid
      traverse(tree)
      isValid = oldValid
    }

    def steps(trees: List[Tree], valid: Boolean): Unit ={
      val oldValid = isValid
      isValid &= valid
      traverseTrees(trees)
      isValid = oldValid
    }

    /** Attempting to mirror the same search criteria as found in TailRec. Will not handle
     *  the case of the "or" or "and" functions ("||" and "&&" respectfully) as I don't
     *  know how that type of conversion can be made.
     *
     *  TODO: Figure out how transform || and &&, if possible.
     */
    override def traverse(tree: Tree): Unit = tree match{
      case Apply(_, args) if search.contains(tree.symbol) => 
        if(isValid) calls += tree.symbol else calls -= tree.symbol
        steps(args, false)
      case Apply(fun, args) => 
        step(fun, false)
        steps(args, false)
      case DefDef(_, _, _, vparams, _, rhs) =>
        vparams.foreach(steps(_, false))
        step(rhs, false)
      case Block(stats, expr) =>
        steps(stats, false)
        step(expr, isValid)
      case If(pred, ftrue, ffalse) =>
        step(pred, false)
        step(ftrue, isValid)
        step(ffalse, isValid)
      case Match(selector, cases) =>
        step(selector, false)
        steps(cases, isValid)
      case Try(block, catches, EmptyTree) =>
        step(block, false) //TODO: Does this mean things surrounded by try{ } aren't tail pos?!
        steps(catches, isValid)
      case Try(block, catches, finalizer) =>
        step(block, false)
        steps(catches, false)
        step(finalizer, false)
      case Select(qual, _) =>
        step(qual, false)
      case Function(vparams, body) =>
        steps(vparams, false)
        step(body, false)
      case UnApply(fun, args) =>
        step(fun, false)
        steps(args, false)
      case ClassDef(_, _, _, body) => step(body, false)
      case ModuleDef(_, _, body) => step(body, false)
      case ValDef(_, _, _, rhs) => step(rhs, false)
      case _ => super.traverse(tree)
    }
  }
}