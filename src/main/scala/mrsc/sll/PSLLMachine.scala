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

sealed trait Extra
// TODO: rename to NoExtra
object DummyExtra extends Extra

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
  val Msg, MinimalGen, AllGens = Value
}

import FoldStrategy._
import RebuilduingStrategy._
import RebuilderTactics._
// P here means "with parameters"
// the only "creative" parameters are: whistle + tricks
class PSLLMultiMachine(
  val program: Program,
  val foldStrategy: FoldStrategy = Ancestors,
  val whistle: Whistle = HEWhistle,
  val speculate: Boolean = true,
  val rebuilduingStrategy: RebuilduingStrategy = CurrentByWhistle,
  val rebuilduingTactics: RebuilderTactics = Msg)
  extends BaseMultiMachine[Expr, SubStepInfo, Extra] {

  val speculator = new Speculator(program)

  def renamingFilter(leaf: CoNode[Expr, SubStepInfo, Extra])(n: CoNode[Expr, SubStepInfo, Extra]) =
    !n.label.isInstanceOf[Var] && SLLExpressions.renaming(leaf.label, n.label)

  def fold(ps: PState[Expr, SubStepInfo, Extra]): List[Path] =
    if (ps.node.in != null && Stop == ps.node.in.label.stepKind) Nil else
      foldStrategy match {
        case All => ps.completeNodes.filter { renamingFilter(ps.node) } map { _.path }
        case _ =>
          ps.node.ancestors.dropWhile { n => n.info.isInstanceOf[SpeculationStep] } match {
            case Nil => Nil
            case n1 :: ns if ps.node.info.isInstanceOf[SpeculationStep] && n1.path == ps.node.in.node.path => ns filter { renamingFilter(ps.node) } map { _.path }
            case ns => ns filter { renamingFilter(ps.node) } map { _.path }
          }
        //case Ancestors => ps.node.ancestors.remove { n => n.extra.isInstanceOf[SpeculationStep] } filter { renamingFilter(ps.node) } map { _.path }
      }

  // TODO: possibly extract into separate module
  def drive(ps: PState[Expr, SubStepInfo, Extra]): List[SubStep[Expr, SubStepInfo, Extra]] = decompose(ps.node.label) match {
    case DecLet(Let(term, bs)) =>
      SubStep(term, LetBodyStep, DummyExtra) :: bs.map { case (k, v) => SubStep(v, LetPartStep(k), DummyExtra) }

    case ObservableCtr(c @ Ctr(_, Nil)) =>
      List(SubStep(c, StopStep, DummyExtra))

    case ObservableCtr(Ctr(_, args)) =>
      args map { a => SubStep(a, CtrArgStep, DummyExtra) }

    case ObservableVar(v) =>
      List(SubStep(v, StopStep, DummyExtra))

    case context @ Context(RedexFCall(FCall(name, args))) =>
      val fReduced = subst(program.f(name).term, Map(program.f(name).args.zip(args): _*))
      val nExpr = context.replaceRedex(fReduced)
      List(SubStep(nExpr, TransientStep, DummyExtra))

    case context @ Context(RedexGCallCtr(GCall(name, args), Ctr(cname, cargs))) =>
      val g = program.g(name, cname)
      val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (cargs ::: args.tail): _*))
      val nExpr = context.replaceRedex(gReduced)
      List(new SubStep(nExpr, TransientStep, DummyExtra))

    case context @ Context(RedexGCallVar(GCall(name, args), v)) =>
      val branches = program.gs(name) map { g =>
        val fp = freshPat(g.p)
        val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (fp.args ::: args.tail): _*))
        val info = Map(v -> Ctr(fp.name, fp.args))
        val driven = subst(context.replaceRedex(gReduced), info)
        new SubStep(driven, VariantBranchStep(Contraction(v, fp)), DummyExtra)
      }
      val sel = new SubStep(v, VariantSelectorStep, DummyExtra)
      sel :: branches

  }

  def blame(pState: PState[Expr, SubStepInfo, Extra]) = {
    val inStep = if (pState.node.in != null) pState.node.in.label else null
    inStep match {
      case StopStep => Blaming(None, Whistle.Complete)
      case _ => whistle.blame(pState) match {
        case None => Blaming(None, Whistle.OK)
        case s @ Some(_) => Blaming(s, Whistle.SoftPrune)
      }
    }
  }

  var toReplace: CoNode[Expr, SubStepInfo, Extra] = null

  def rebuildings(pState: PState[Expr, SubStepInfo, Extra], signal: Blaming[Expr, SubStepInfo, Extra]) =
    signal.signal match {
      case Whistle.SoftPrune =>
        rebuilduingStrategy match {
          case CurrentByWhistle => {
            rebuilduingTactics match {
              case Msg =>
                val blamedNode = signal.blamed.get
                val blamedExpr = blamedNode.label
                val currentExpr = pState.node.label
                //println(currentExpr)
                //println(blamedExpr)
                val rebuilt = msgToLet(currentExpr, MSG.msg(currentExpr, blamedExpr))
                //println(rebuilt)
                //println("----")
                List(SubStep(rebuilt, GeneralizationStep(currentExpr), DummyExtra))
              case AllGens => {
                val blamedNode = pState.node
                toReplace = blamedNode
                val e = blamedNode.label
                SLLGeneralizations.gens(blamedNode.label) map { e1 => new SubStep(e1, GeneralizationStep(e), DummyExtra) }
              }
            }
          }
          case DangerousByWhistle =>
            rebuilduingTactics match {
              case Msg => {
                val blamedNode = signal.blamed.get
                val blamedExpr = blamedNode.label
                val currentExpr = pState.node.label
                println(currentExpr)
                println(blamedExpr)
                val rebuilt = msgToLet(blamedExpr, MSG.msg(blamedExpr, currentExpr))
                println(rebuilt)
                println("----")
                toReplace = blamedNode
                List(SubStep(rebuilt, GeneralizationStep(blamedExpr), DummyExtra))
              }
              case AllGens => {
                val blamedNode = signal.blamed.get
                toReplace = blamedNode
                val e = blamedNode.label
                SLLGeneralizations.gens(blamedNode.label) map { e1 => new SubStep(e1, GeneralizationStep(e), DummyExtra) }
              }
            }
          case _ => Nil
        }
      case _ => Nil
    }

  def rebuildStep(gs: SubStep[Expr, SubStepInfo, Extra]) = //MForest(List(gs))
    rebuilduingStrategy match {
      case CurrentByWhistle =>
        // TODO: should we do something here?
        MReplace(gs.label, DummyExtra)
      case DangerousByWhistle =>
        println("returning rollback")
        MRollback(toReplace, gs.label, DummyExtra)
    }

  def msgToLet(ce: Expr, g: Gen) = {
    if (g.t.isInstanceOf[Var]) {
      throw new Error("Please check")
    }
    if (renaming(g.t, ce)) {
      split(ce)
    } else {
      Let(g.t, g.m1.toList)
    }
  }

  def split(e: Expr): Expr = {
    e match {
      case Ctr(name, args) => {
        val vs = args map freshVar
        Let(Ctr(name, vs), vs zip args)
      }
      case FCall(name, args) => {
        val vs = args map freshVar
        Let(FCall(name, vs), vs zip args)
      }
      case GCall(name, args) => {
        val vs = args map freshVar
        Let(GCall(name, vs), vs zip args)
      }
    }
  }

  def tricks(pState: PState[Expr, SubStepInfo, Extra], signal: Blaming[Expr, SubStepInfo, Extra]): List[SubStep[Expr, SubStepInfo, Extra]] = {
    if (speculate) {
      val from = pState.node.label
      val res = speculator.speculate(from) map { e => SubStep(e, SpeculationStep(from, e), DummyExtra) }
      res
    } else {
      Nil
    }
  }

  def trickyStep(gs: SubStep[Expr, SubStepInfo, Extra]): Step[Expr, SubStepInfo, Extra] =
    MForest(List(gs))

  private def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}