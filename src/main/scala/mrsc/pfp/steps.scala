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
case object CtrArgLabel extends Label {
  override def toString = ""
}
case object TermSkelLabel extends Label {
  override def toString = "let"
}
case class SubTermLabel(t: Term) extends Label {
  override def toString = t + "->"
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
case class DecomposeMStep[C](parts: List[MetaTerm]) extends MStep {
  val graphStep = AddChildNodesStep[MetaTerm, Label](parts map { (_, CtrArgLabel) })
}
case class VariantsMStep(sel: Term, cases: List[(Ptr, Ctr, Term)]) extends MStep {
  val graphStep = {
    val ns = cases map { v => (v._3, CaseBranchLabel(sel, v._1, v._2)) }
    AddChildNodesStep[MetaTerm, Label](ns)
  }
}
