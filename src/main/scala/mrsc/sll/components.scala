package mrsc.sll

import mrsc._
import Decomposition._
import SLLExpressions._

object StepKind extends Enumeration {
  type StepKind = Value
  val Transient, CtrDecompose, LetDecompose, Variants, Generalization, Speculation = Value

  def isDrive(v: Value) =
    v == Transient || v == Variants || v == CtrDecompose || v == LetDecompose

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

sealed trait Extra
// TODO: rename to NoExtra
object DummyExtra extends Extra

trait SLLDriving {
  val program: Program

  def isLeaf(pState: SLLState) = pState.node.conf match {
    case Var(_) => true
    case Ctr(_, Nil) => true
    case _ => false
  }

  def drive(ps: SLLState): SLLStep =
    MAddForest(drive_(ps.node.conf))

  private def drive_(conf: Expr): List[SubStep[Expr, SubStepInfo, Extra]] =
    decompose(conf) match {

      case DecLet(Let(term, bs)) =>
        SubStep(term, LetBodyStep, DummyExtra) :: bs.map {
          case (k, v) => SubStep(v, LetPartStep(k), DummyExtra)
        }

      case ObservableCtr(Ctr(_, args)) =>
        args map { a => SubStep(a, CtrArgStep, DummyExtra) }

      case ObservableVar(v) =>
        List()

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

  private def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}

// fold only into ancestors
trait SLLFolding[D, E] {

  import StepKind._
  def fold(ps: PState[Expr, SubStepInfo, Extra]): Option[Path] =
    ps.node.ancestors.find { renamingFilter(ps.node) } map { _.path }

  private def renamingFilter(leaf: CoNode[Expr, _, _])(n: CoNode[Expr, _, _]) =
    SLLExpressions.renaming(leaf.conf, n.conf)
}

trait SLLWhistle {
  val whistle: Whistle

  def blame(pState: PState[Expr, SubStepInfo, Extra]): Blaming[Expr, SubStepInfo, Extra] =
    whistle.blame(pState) match {
      case None => Blaming(None, Whistle.OK)
      case s @ Some(_) => Blaming(s, Whistle.Warning)
    }
}

trait SLLRebuildings {
	  def msg(conf: Expr, wrt: Expr): Expr = {
	    println("msg requested")
	    println(conf)
	    println(wrt)
	    msgToLet(conf, MSG.msg(conf, wrt))
	  }

	  def gens(conf: Expr): List[Expr] =
	    SLLGeneralizations.gens(conf)

	  private def msgToLet(ce: Expr, g: Gen) =
	    if (renaming(g.t, ce) || g.t.isInstanceOf[Var]) {
	      split(ce)
	    } else {
	      Let(g.t, g.m1.toList)
	    }

	  private def split(e: Expr): Expr =
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

