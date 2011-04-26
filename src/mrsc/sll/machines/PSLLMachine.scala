package mrsc.sll

import mrsc._
import Decomposition._
import SLLExpressions._

object StepKind extends Enumeration {
  type StepKind = Value
  val Stop, Transient, CtrDecompose, LetDecompose, Variants, Generalization, Speculation = Value

  def isDrive(v: Value) =
    v == Stop || v == Transient || v == Variants || v == CtrDecompose || v == LetDecompose

  def isReduction(v: Value) =
    v == Transient || v == Variants
}

import StepKind._

abstract sealed class SubStepInfo(val stepKind: StepKind)
case object TransientStep extends SubStepInfo(Transient) {
  override def toString = " "
}
case object CtrArgStep extends SubStepInfo(CtrDecompose) {
  override def toString = ""
}
case object VariantSelectorStep extends SubStepInfo(Variants) {
  override def toString = "[ ]"
}
case class VariantBranchStep(contr: Contraction) extends SubStepInfo(Variants) {
  override def toString = contr.toString
}
case object LetBodyStep extends SubStepInfo(LetDecompose)
case class LetPartStep(v: Var) extends SubStepInfo(LetDecompose)
case class GeneralizationStep(from: Expr) extends SubStepInfo(Generalization)
case class SpeculationStep(from: Expr, to: Expr) extends SubStepInfo(Speculation) {
  override def toString = from.toString + " -> " + to.toString
}
case object StopStep extends SubStepInfo(Stop) {
  override def toString = "!"
}

object FoldStrategy extends Enumeration {
  type FoldStrategy = Value
  val All, Ancestors = Value
}

object RebuilduingStrategy extends Enumeration {
  type RebuilduingStrategy = Value
  val Never, DangerousByWhistle, CurrentByWhistle, Always = Value
}

object RebuilderTactics extends Enumeration {
  type RebuilderTactics = Value
  val Msg, MinimalGen, Gen = Value
}

import FoldStrategy._
// P here means "with parameters"
class PSLLMultiMachine(
  val program: Program,
  val foldStrategy: FoldStrategy = Ancestors,
  val whistle: Whistle = HEWhistle,
  val speculate: Boolean = false) extends BaseMultiMachine[Expr, SubStepInfo] {

  val speculator = new Speculator(program)

  def renamingFilter(leaf: CoNode[Expr, SubStepInfo])(n: CoNode[Expr, SubStepInfo]) =
    !n.configuration.isInstanceOf[Var] && SLLExpressions.renaming(leaf.configuration, n.configuration)

  def fold(ps: PState[Expr, SubStepInfo]): List[Path] =
    if (Stop == ps.node.info.stepKind) Nil else
      foldStrategy match {
        case All => ps.completeNodes.filter { renamingFilter(ps.node) } map { _.path }
        case _ =>
          ps.node.ancestors.dropWhile { n => n.info.isInstanceOf[SpeculationStep] } match {
            case Nil => Nil
            case n1 :: ns if ps.node.info.isInstanceOf[SpeculationStep] && n1.path == ps.node.parent.path => ns filter { renamingFilter(ps.node) } map { _.path }
            case ns => ns filter { renamingFilter(ps.node) } map { _.path }
          }
        //case Ancestors => ps.node.ancestors.remove { n => n.info.isInstanceOf[SpeculationStep] } filter { renamingFilter(ps.node) } map { _.path }
      }

  // TODO: possibly extract into separate module
  def drive(ps: PState[Expr, SubStepInfo]): List[SubStep[Expr, SubStepInfo]] = decompose(ps.node.configuration) match {
    case DecLet(Let(term, bs)) =>
      new SubStep(term, LetBodyStep) :: bs.map { case (k, v) => new SubStep(v, LetPartStep(k)) }

    case ObservableCtr(c @ Ctr(_, Nil)) =>
      List(new SubStep(c, StopStep))

    case ObservableCtr(Ctr(_, args)) =>
      args map { a => new SubStep(a, CtrArgStep) }

    case ObservableVar(v) =>
      List(new SubStep(v, StopStep))

    case context @ Context(RedexFCall(FCall(name, args))) =>
      val fReduced = subst(program.f(name).term, Map(program.f(name).args.zip(args): _*))
      val nExpr = context.replaceRedex(fReduced)
      List(new SubStep(nExpr, TransientStep))

    case context @ Context(RedexGCallCtr(GCall(name, args), Ctr(cname, cargs))) =>
      val g = program.g(name, cname)
      val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (cargs ::: args.tail): _*))
      val nExpr = context.replaceRedex(gReduced)
      List(new SubStep(nExpr, TransientStep))

    case context @ Context(RedexGCallVar(GCall(name, args), v)) =>
      val branches = program.gs(name) map { g =>
        val fp = freshPat(g.p)
        val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (fp.args ::: args.tail): _*))
        val info = Map(v -> Ctr(fp.name, fp.args))
        val driven = subst(context.replaceRedex(gReduced), info)
        new SubStep(driven, VariantBranchStep(Contraction(v, fp)))
      }
      val sel = new SubStep(v, VariantSelectorStep)
      sel :: branches

  }

  def terminate(pState: PState[Expr, SubStepInfo]): Whistle.Value = pState.node.info match {
    case StopStep => Whistle.Complete
    case _ => if (whistle.accept(pState)) Whistle.OK else {
      /*
      println("***")
      println(pState.node.configuration)
      for (as <- pState.node.ancestors) {
        println(as.configuration)
      }
      println("***")
      */
      Whistle.SoftPrune
    }
  }

  def rebuildings(pState: PState[Expr, SubStepInfo], signal: Whistle.Value) =
    Nil

  def rebuildStep(gs: SubStep[Expr, SubStepInfo]) =
    null

  def tricks(pState: PState[Expr, SubStepInfo], signal: Whistle.Value): List[SubStep[Expr, SubStepInfo]] = {
    if (speculate) {
      val from = pState.node.configuration
      val res = speculator.speculate(from) map { e => SubStep(e, SpeculationStep(from, e)) }
      if (!res.isEmpty) {
        //println(res)
      }
      res
    } else {
      Nil
    }
  }

  def trickyStep(gs: SubStep[Expr, SubStepInfo]): MStep[Expr, SubStepInfo] =
    MForest(List(gs))

  private def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}