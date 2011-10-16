package mrsc.pfp

import mrsc.core._

case class Contraction[C](v: Name, pat: C) {
  override def toString =
    if (v != null) v + " = " + pat else ""
  def subst() = Map[Name, C](v -> pat)
}

sealed trait DriveInfo[C]
case class TransientStepInfo[C] extends DriveInfo[C] {
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

case class StopDriveStep[C] extends DriveStep[C] {
  val graphStep =
    CompleteCurrentNodeStep[C, DriveInfo[C]]()
}

case class TransientDriveStep[C](next: C) extends DriveStep[C] {
  val graphStep = {
    val ns = List((next, TransientStepInfo[C]()))
    AddChildNodesStep[C, DriveInfo[C]](ns)
  }
}

case class DecomposeDriveStep[C](compose: List[C] => C, parts: List[C]) extends DriveStep[C] {
  val graphStep = {
    val ns = parts map { (_, DecomposeStepInfo(compose)) }
    AddChildNodesStep[C, DriveInfo[C]](ns)
  }
}

case class VariantsDriveStep[C](cases: List[(C, Contraction[C])]) extends DriveStep[C] {
  val graphStep = {
    val ns = cases map { v => (v._1, VariantsStepInfo(v._2)) }
    AddChildNodesStep[C, DriveInfo[C]](ns)
  }
}