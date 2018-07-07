package twotails

import scala.annotation.{switch, tailrec}
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.symtab.Flags._
import collection.mutable.{Map => MMap, Set => MSet}

class TwoTailsPlugin(val global: Global) extends Plugin{
  val name = "twotails"
  val description = "Adds support for mutually recursive functions in Scala."
  var limitSize = true

  override def init(options: List[String], error: String => Unit): Boolean ={
    options.foreach{ 
      case "size" => limitSize = true
      case "memory" => limitSize = false
      case opt => error(s"Option $opt is not a valid compiler option for TwoTails.")
    }
    true
  }

  //see: https://github.com/scala/scala/blob/2.12.x/src/compiler/scala/tools/nsc/plugins/Plugin.scala
  override val optionsHelp: Option[String] = Some{
    "-P:twotails:size >> The default setting, transforms mutual recursion based on JVM method size limitations.\n" +
    "-P:twotails:memory >> Transforms mutual recursion so as to limit allocation overhead. Use with caution."
  }

  val components = List[PluginComponent](new MutualRecComponent(global, { () => limitSize }))
}

final class MutualRecComponent(val global: Global, limitSize: () => Boolean) 
    extends PluginComponent with SizeLimited {
  import global._

  val phaseName = "twotails"
  override val runsBefore = List("tailcalls", "uncurry", "patmat")
  val runsAfter = List("typer")//, "patmat")

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

    def hasMutualRec(tree: Tree) = 
      (tree.symbol != NoSymbol) &&
      tree.symbol.annotations.exists(_.tpe.typeSymbol == mtrec)

    //In case there's just one, convert to a @tailrec. No reason to add a whole new method.
    def convertTailRec(body: List[Tree]): Unit = body.foreach{
      case ddef: DefDef if hasMutualRec(ddef) => 
        ddef.symbol
          .removeAnnotation(mtrec)
          .withAnnotations(trec :: Nil)
      case _ => 
    }

    private lazy val mutualMethod = if(limitSize()) new SizeLimitedMethod(localTyper) 
      else new AllocationLimitedMethod(localTyper)

    def convertMutualRec(root: Tree, body: List[Tree]): List[Tree] ={
      val (recs, everythingElse) = body.partition{
        case ddef: DefDef => hasMutualRec(ddef)
        case _ => false
      }
      val symbols: Map[Symbol, Tree] = recs.map{ t => (t.symbol, t) }.toMap
      val walker = new CallGraphWalker(symbols.keySet, !limitSize())
      val adjacencyList: Map[Symbol, List[Symbol]] = recs.map{ tree =>
        val calls = walker.walk(tree)
        (tree.symbol, calls)
      }.toMap
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
          .withAnnotations(trec :: Nil)
          head :: Nil
        case defs =>
          val (methSym, methTree) = mutualMethod(root, defs, TermName("mutualrec_fn$" + tndx))
          val forwardedTrees = forwardTrees(methSym, defs)
          tndx += 1
          methTree :: forwardedTrees
      }

      if(ungrouped.isEmpty) everythingElse ::: optimized
      else everythingElse ::: ungrouped ::: optimized
    }

    def forwardTrees(methSym: Symbol, defdef: List[Tree]): List[Tree] = defdef.zipWithIndex.map{
      case (tree, indx) =>
        val DefDef(_, _, _, vp :: vps, _, _) = tree
        val newVp = localTyper.typed(Literal(Constant(indx))) :: vp.map(p => gen.paramToArg(p.symbol))
        val refTree = gen.mkAttributedRef(tree.symbol.owner.thisType, methSym)
        val forwarderTree = vps.foldLeft(Apply(refTree, newVp)){
          (fn, params) => Apply(fn, params map (p => gen.paramToArg(p.symbol)))
        }
        val forwarded = deriveDefDef(tree)(_ => localTyper.typedPos(tree.symbol.pos)(forwarderTree))
        if(tree.symbol.isEffectivelyFinalOrNotOverridden) forwarded.symbol.removeAnnotation(mtrec)
        forwarded
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
  }

    class AllocationLimitedMethod(localTyper: analyzer.Typer) extends MutualMethod{
      def mkNewMethodSymbol(symbol: Symbol, name: TermName): Symbol ={
        val flags = METHOD | FINAL | PRIVATE | ARTIFACT
        val methSym = symbol.cloneSymbol(symbol.owner, flags, name)
        val param = methSym.newSyntheticValueParam(definitions.IntTpe, TermName("indx"))
      
        methSym.modifyInfo {
          case GenPolyType(tparams, MethodType(params, res)) => GenPolyType(tparams, MethodType(param :: params, res))
        }
        methSym
          .removeAnnotation(mtrec)
          .withAnnotations(trec :: Nil)
        localTyper.namer.enterInScope(methSym)
      }

      def mkNewMethodRhs(methSym: Symbol, defdef: List[Tree]): Tree ={
        val defSymbols: Map[Symbol, () => Tree] = defdef.zipWithIndex.map{
          case (d, i) => (d.symbol, {() => localTyper.typed(Literal(Constant(i)))})
        }.toMap
        val callTransformer = new AllocCallTransformer(methSym, defSymbols)
        val indxSym :: paramSymbols = methSym.info.paramss.flatten
        val defrhs = defdef.map{ tree =>
          val DefDef(_, _, _, vparams, _, rhs) = tree

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
          val neww = deskolemized ::: methSym.typeParams ::: paramSymbols
        
          callTransformer.transform(rhs)
            .changeOwner(tree.symbol -> methSym)
            .substituteSymbols(old, neww)
        }

        val cases = defrhs.zipWithIndex.map{
          case (body, i) => cq"$i => $body"
        }

        val indxRef = gen.mkAttributedRef(indxSym) setType definitions.IntTpe
        q"($indxRef: @scala.annotation.switch) match{ case ..$cases }"
      }

      def mkNewMethodTree(methSym: Symbol, tree: Tree, rhs: Tree): Tree =
        localTyper.typedPos(tree.pos){
          DefDef(methSym, rhs)
        }
    }

  /** @notes The newly created method symbol can be reused without issue but if the symbol `Tree`
   *         of the index literals are reused, creates a `NullPointerException` during Erasure 
   *         phase.
   */
  final class AllocCallTransformer(methSym: Symbol, symbols: Map[Symbol, () => Tree]) extends Transformer{
    private val ref = gen.mkAttributedRef(methSym)
    private val bor = currentRun.runDefinitions.Boolean_or
    private val band = currentRun.runDefinitions.Boolean_and

    override def transform(tree: Tree): Tree = tree match{
      case Apply(fn, _) if symbols.contains(fn.symbol) => multiArgs(tree)
      case Apply(fn, args) if fn.symbol == band || fn.symbol == bor =>
        treeCopy.Apply(tree, fn, transformTrees(args))
      case Block(stats, expr) => treeCopy.Block(tree, stats, transform(expr))
      case If(pred, ftrue, ffalse) => treeCopy.If(tree, pred, transform(ftrue), transform(ffalse))
      case Match(selector, cases) => 
        treeCopy.Match(tree, selector, transformTrees(cases).asInstanceOf[List[CaseDef]])
      case Try(block, catches, EmptyTree) => 
        treeCopy.Try(tree, block, transformTrees(catches).asInstanceOf[List[CaseDef]], EmptyTree)
      case that: Apply => that
      case that: Try => that
      case that: Select => that
      case that: DefDef => that
      case that: ClassDef => that
      case that: ModuleDef => that
      case _ => super.transform(tree)
    }

    def multiArgs(tree: Tree): Tree = tree match{
      case Apply(fn, args) => 
        multiArgs(fn) match{
          case f @ Apply(_, _) => treeCopy.Apply(tree, f, transformTrees(args))
          case f => treeCopy.Apply(tree, f, symbols(fn.symbol)() :: transformTrees(args))
        }
      case TypeApply(fn, targs) => treeCopy.TypeApply(tree, ref, targs) setType (ref.tpe)
      case _ => ref
    }
  }

  //Searches the Tree and returns all functions found in tailcall position from a search set.
  final class CallGraphWalker(search: Set[Symbol], andOr: Boolean) extends Traverser{
    private final val calls = MSet.empty[Symbol]
    private var isValid = true

    private val bor = currentRun.runDefinitions.Boolean_or
    private val band = currentRun.runDefinitions.Boolean_and

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
     *  TODO: Figure out how transform || and && for size-limited transformation.
     */
    override def traverse(tree: Tree): Unit = tree match{
      case Apply(_, args) if search.contains(tree.symbol) => 
        if(isValid) calls += tree.symbol else calls -= tree.symbol
        steps(args, false)
      case Apply(fun, args) if fun.symbol == bor || fun.symbol == band =>
        step(fun, false)
        steps(args, isValid && andOr)
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