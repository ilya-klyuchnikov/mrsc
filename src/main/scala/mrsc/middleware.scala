package mrsc

case class Contraction[C](v: Name, pat: C) {
  override def toString = v + " = " + pat
}

abstract sealed trait DriveStep[+C]
case class TransientDriveStep[C](next: C) extends DriveStep[C]
case object StopDriveStep extends DriveStep[Nothing]
case class DecomposeDriveStep[C](compose: List[C] => C, parts: List[C]) extends DriveStep[C]
case class VariantsDriveStep[C](cases: List[(Contraction[C], C)]) extends DriveStep[C]

abstract sealed class DriveInfo[+C]
case object TransientStepInfo extends DriveInfo[Nothing] {
  override def toString = "->"
}
case class DecomposeStepInfo[C](compose: List[C] => C) extends DriveInfo[C] {
  override def toString = ""
}
case class VariantsStepInfo[C](contr: Contraction[C]) extends DriveInfo[C] {
  override def toString = contr.toString
}

sealed trait Extra
case object NoExtra extends Extra
