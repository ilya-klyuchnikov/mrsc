package mrsc.pfp

import mrsc.core._

case class Contraction[+C](v: Name, pat: C) {
  override def toString =
    if (v != null) v + " = " + pat else ""
  def subst() = Map[Name, C](v -> pat)
}

sealed trait DriveInfo[+C]
case object TransientStepInfo extends DriveInfo[Nothing] {
  override def toString = "->"
}
case class DecomposeStepInfo[C](compose: List[C] => C) extends DriveInfo[C] {
  override def toString = ""
}
case class VariantsStepInfo[C](contr: Contraction[C]) extends DriveInfo[C] {
  override def toString = contr.toString
}

sealed trait DriveStep[C] {
  val graphStep: GraphStep[C, DriveInfo[C]]
}

case class TransientDriveStep[C](next: C) extends DriveStep[C] {
  val graphStep = {
    val subSteps = List((next, TransientStepInfo)): List[(C, DriveInfo[C])]
    AddChildNodesStep(subSteps)
  }
}

case class StopDriveStep[C] extends DriveStep[C] {
  val graphStep: GraphStep[C, DriveInfo[C]] =
    CompleteCurrentNodeStep()
}

case class DecomposeDriveStep[C](compose: List[C] => C, parts: List[C]) extends DriveStep[C] {
  val graphStep: GraphStep[C, DriveInfo[C]] = {
    val stepInfo = DecomposeStepInfo(compose)
    val subSteps = parts map { a => (a, stepInfo) }
    AddChildNodesStep(subSteps)
  }
}

case class VariantsDriveStep[C](cases: List[(C, Contraction[C])]) extends DriveStep[C] {
  val graphStep: GraphStep[C, DriveInfo[C]] = {
    val ns = cases map { v => (v._1, VariantsStepInfo(v._2)) }
    AddChildNodesStep(ns)
  }
}