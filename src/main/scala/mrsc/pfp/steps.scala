package mrsc.pfp

import mrsc.core._

// Label b/c labeled transition system.
// We put it on graph edges.
sealed trait Label
case object TransientLabel extends Label {
  override def toString = "->"
}
case class CaseBranchLabel(sel: Term, ptr: Ptr, alt: Ctr) extends Label {
  override def toString = sel + " = " + alt
}
case object CaseSelLabel extends Label
case class DecomposeLabel(compose: List[Term] => Term) extends Label {
  override def toString = ""
}

// MetaStep = Step of supercompiler.
// Transformed into graph rewrite step.
sealed trait MStep {
  val graphStep: GraphRewriteStep[MetaTerm, Label]
}
case class TransientMStep(next: MetaTerm) extends MStep {
  val graphStep = AddChildNodesStep[MetaTerm, Label](List((next, TransientLabel)))
}
case object StopMStep extends MStep {
  val graphStep = CompleteCurrentNodeStep[MetaTerm, Label]()
}
case class DecomposeCtrMStep(ctr: Ctr) extends MStep {
  val compose = (args: List[Term]) => Ctr(ctr.name, args)
  val graphStep = AddChildNodesStep[MetaTerm, Label](ctr.args map { (_, DecomposeLabel(compose)) })
}
case class DecomposeAbsMStep(body: Term, fv: FVar) extends MStep {
  import Syntax._
  val compose = (ls: List[Term]) => Abs(applySubst(termShift(1, ls.head), Map(fv -> BVar(0))))
  val graphStep = AddChildNodesStep[MetaTerm, Label](List((body, DecomposeLabel(compose))))
}
case class DecomposeVarApp(fv: FVar, args: List[Term]) extends MStep {
  val compose = (ls: List[Term]) => ls.reduce(App)
  val graphStep = AddChildNodesStep[MetaTerm, Label]((fv :: args) map { (_, DecomposeLabel(compose)) })
}
case class VariantsMStep(sel: Term, cases: List[(Ptr, Ctr, Term)]) extends MStep {
  val graphStep = {
    val ns = cases map { v => (v._3, CaseBranchLabel(sel, v._1, v._2)) }
    AddChildNodesStep[MetaTerm, Label](ns)
  }
}

case class DecomposeRebuildingMStep(t: Rebuilding) extends MStep {
  val parts = t.sub.toList
  val vals = parts.map { _._2 }
  val fvs = parts.map { _._1 }
  val compose = { (args: List[Term]) =>
    val sub1 = Map(fvs zip args.tail: _*)
    Syntax.applySubst(args.head, sub1)
  }
  val graphStep = AddChildNodesStep[MetaTerm, Label]((t.t :: vals) map { (_, DecomposeLabel(compose)) })
}
