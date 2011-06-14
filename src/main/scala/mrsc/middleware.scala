package mrsc

import mrsc.sll._

case class Contraction[C](v: Name, pat: C) {
  override def toString = v + " = " + pat
}

object StepKind extends Enumeration {
  type StepKind = Value
  val Transient, Decompose, Variants = Value
}

import StepKind._

// TODO: rename later
abstract sealed class DriveInfo[+C](val stepKind: StepKind)
case object TransientStep extends DriveInfo[Nothing](Transient) {
  override def toString = "->"
}
case class DecomposeStep[C](compose: List[C] => C) extends DriveInfo[C](Decompose) {
  override def toString = ""
}
case class VariantBranchStep[C](contr: Contraction[C]) extends DriveInfo[C](Variants) {
  override def toString = contr.toString
}

sealed trait Extra
case object NoExtra extends Extra
