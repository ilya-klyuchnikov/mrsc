package mrsc.pfp

import mrsc.core._
import java.lang.reflect.InvocationTargetException

// PART 1. Labels.
// TODO: really it is some kind of LTS.
// It may be worth to reformulate it in terms of LTS.
// Labels or actions of our LTS.
// We put them on graph edges.
sealed trait Label
case object TransientLabel extends Label {
  override def toString = "->"
}
case object UnfoldLabel extends Label {
  override def toString = "->*"
}
case class CaseBranchLabel(sel: Term, ptr: Ptr, alt: Ctr) extends Label {
  override def toString = sel + " = " + alt
}
case class DecomposeLabel(compose: List[Term] => Term) extends Label {
  override def toString = ""
}

// PART 2. Meta steps.
// MetaStep = Step of supercompiler.
// Transformed into graph rewrite step.
// Meta steps are just intermediate data structures.
// They allow to override tactics in traits. For example:
// by default, driving is implemented without positive information
// propagation. In the specialized trait we can perform
// information propagation.
sealed trait MStep {
  val graphStep: GraphRewriteStep[MetaTerm, Label]
}
// Reduction
case class TransientMStep(next: MetaTerm) extends MStep {
  val graphStep = AddChildNodesStep[MetaTerm, Label](List((next, TransientLabel)))
}
// Reduction via unfolding of a function definition
case class UnfoldMStep(next: MetaTerm) extends MStep {
  val graphStep = AddChildNodesStep[MetaTerm, Label](List((next, UnfoldLabel)))
}
// Rebuilding (generalization)
case class RebuildMStep(rb: Rebuilding) extends MStep {
  val graphStep = RebuildStep[MetaTerm, Label](rb)
}
// Stop - no more reductions (variable, nullary constructors)
case object StopMStep extends MStep {
  val graphStep = CompleteCurrentNodeStep[MetaTerm, Label]()
}
// Drilling into arguments of constructor.
case class DecomposeCtrMStep(ctr: Ctr) extends MStep {
  val compose = (args: List[Term]) => Ctr(ctr.name, args)
  val graphStep = AddChildNodesStep[MetaTerm, Label](ctr.args map { (_, DecomposeLabel(compose)) })
}
// Drilling into body of lambda abstraction.
case class DecomposeAbsMStep(body: Term, fv: FVar) extends MStep {
  import NamelessSyntax._
  val compose = (ls: List[Term]) => Abs(applySubst(termShift(1, ls.head), Map(fv -> BVar(0))))
  val graphStep = AddChildNodesStep[MetaTerm, Label](List((body, DecomposeLabel(compose))))
}
// Application of an unknown function. Drilling into arguments.
case class DecomposeVarApp(fv: FVar, args: List[Term]) extends MStep {
  val compose = (ls: List[Term]) => ls.reduce(App(_, _))
  val graphStep = AddChildNodesStep[MetaTerm, Label]((fv :: args) map { (_, DecomposeLabel(compose)) })
}
// Pattern matching.
case class VariantsMStep(sel: FVar, cases: List[(Ptr, Ctr, Term)]) extends MStep {
  val graphStep = {
    val ns = cases map { v => (v._3, CaseBranchLabel(sel, v._1, v._2)) }
    AddChildNodesStep[MetaTerm, Label](ns)
  }
}
// Drilling into parts of rebuilding.
case class DecomposeRebuildingMStep(t: Rebuilding) extends MStep {
  import NamelessSyntax._
  val parts = t.sub.toList
  val vals = parts.map { _._2 }
  val fvs = parts.map { _._1 }
  val compose = { (args: List[Term]) =>
    val sub1 = Map(fvs zip args.tail: _*)
    applySubst(args.head, sub1)
  }
  val graphStep = AddChildNodesStep[MetaTerm, Label]((t.t :: vals) map { (_, DecomposeLabel(compose)) })
}
// Drilling into parts of rebuilding. But compose function
// do not perform substitution - it creates let-expression.
case class FreezeRebuildingMStep(t: Rebuilding) extends MStep {
  import NamelessSyntax._
  val parts = t.sub.toList
  val vals = parts.map { _._2 }
  val fvs = parts.map { _._1 }
  val compose = { (args: List[Term]) =>
    (fvs zip args.tail).foldLeft(args.head){(acc, p) => Let(p._2, applySubst(acc, Map(p._1 -> BVar(0))))}
  }
  val graphStep = AddChildNodesStep[MetaTerm, Label]((t.t :: vals) map { (_, DecomposeLabel(compose)) })
}

// It is an open question how to get rid off this
// global state and to not over-complicate passing freevar.
trait VarGen {
  var freeVar: Int = 100
  def nextVar(x: Any = ()): FVar = {
    freeVar += 1
    FVar(freeVar)
  }
}

// Driving without positive information propagation
trait PFPSemantics extends VarGen {
  import NamelessSyntax._
  val gc: GContext
  def driveStep(t: MetaTerm): MStep = t match {
    case rb: Rebuilding =>
      DecomposeRebuildingMStep(rb)
    case t: Term => Decomposition.decompose(t) match {
      case ObservableVar(v) =>
        StopMStep
      case ObservableCon(c) =>
        DecomposeCtrMStep(c)
      case ObservableAbs(l) =>
        val fv = nextVar()
        val body1 = termSubstTop(fv, l.t)
        DecomposeAbsMStep(body1, fv)
      case ObservableVarApp(fv, args) =>
        DecomposeVarApp(fv, args)
      case context @ Context(RedexCall(f)) =>
        UnfoldMStep(context.replaceHole(gc(f.n)))
      case context @ Context(RedexFix(t1)) =>
        UnfoldMStep(context.replaceHole(termSubstTop(t1, t1.t)))
      case context @ Context(RedexLamApp(Abs(t1, _), App(_, t2, _))) =>
        TransientMStep(context.replaceHole((termSubstTop(t2, t1))))
      case context @ Context(RedexCaseCtr(Ctr(name, args, _), Case(_, bs, _))) =>
        val Some((ptr, body)) = bs.find(_._1.name == name)
        val next = args.foldRight(body)(termSubstTop(_, _))
        TransientMStep(context.replaceHole(next))
      case context @ Context(RedexCaseAlt(v: FVar, Case(_, bs, _))) =>
        val xs = for { (ptr @ Ptr(name, args), body) <- bs } yield {
          val ctr = Ctr(name, args.map(nextVar))
          val next = ctr.args.foldRight(body)(termSubstTop(_, _))
          (ptr, ctr, context.replaceHole(next))
        }
        VariantsMStep(v, xs)
      // variable application
      case context @ Context(RedexCaseAlt(sel, Case(_, bs, _))) =>
        val v = nextVar()
        RebuildMStep(Rebuilding(context.replaceHole(Case(v, bs)), Map(v -> sel)))
      case context @ Context(RedexLet(Let(v, body, _))) =>
        val red1 = termSubstTop(v, body)
        TransientMStep(context.replaceHole(red1))
    }
  }
}

trait SimplePartialOrdering[T] extends PartialOrdering[T] {
  override def tryCompare(x: T, y: T): Option[Int] = (lteq(x, y), lteq(y, x)) match {
    case (false, false) =>
      None
    case (false, true) =>
      Some(1)
    case (true, false) =>
      Some(-1)
    case (true, true) =>
      Some(0)
  }
}
