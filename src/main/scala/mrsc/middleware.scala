package mrsc

case class Contraction[C](v: Name, pat: C) {
  override def toString = v + " = " + pat
}

object StepKind extends Enumeration {
  type StepKind = Value
  val Transient, Decompose, Variants = Value
}

import StepKind._

abstract sealed class DriveInfo[+C](val stepKind: StepKind)
case object TransientStepInfo extends DriveInfo[Nothing](Transient) {
  override def toString = "->"
}
case class DecomposeStepInfo[C](compose: List[C] => C) extends DriveInfo[C](Decompose) {
  override def toString = ""
}
case class VariantStepInfo[C](contr: Contraction[C]) extends DriveInfo[C](Variants) {
  override def toString = contr.toString
}

sealed trait Extra
case object NoExtra extends Extra
