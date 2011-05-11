package mrsc

object Signal extends Enumeration {
  type Signal = Value
  val OK, Warning = Value
}

import Signal._

// This is middleware between semantics and graph steps
case class Blaming[C, D, E](blamed: Option[CoNode[C, D, E]], signal: Signal)
case class Contraction[C](v: Name, pat: C) {
  override def toString = v + " = " + pat
}

object StepKind extends Enumeration {
  type StepKind = Value
  val Transient, CtrDecompose, LetDecompose, Variants, Generalization = Value

  def isDrive(v: Value) =
    v == Transient || v == Variants || v == CtrDecompose || v == LetDecompose

  def isReduction(v: Value) =
    v == Transient || v == Variants
}

import StepKind._

abstract sealed class SubStepInfo[+C](val stepKind: StepKind)
case object TransientStep extends SubStepInfo[Nothing](Transient) {
  override def toString = " "
}
case object CtrArgStep extends SubStepInfo[Nothing](CtrDecompose) {
  override def toString = ""
}
case object VariantSelectorStep extends SubStepInfo[Nothing](Variants) {
  override def toString = "[ ]"
}
case class VariantBranchStep[C](contr: Contraction[C]) extends SubStepInfo[C](Variants) {
  override def toString = contr.toString
}
case object LetBodyStep extends SubStepInfo(LetDecompose)
case class LetPartStep(v: Name) extends SubStepInfo(LetDecompose)
case class GeneralizationStep[C](from: C) extends SubStepInfo(Generalization)

sealed trait Extra
// TODO: rename to NoExtra
object DummyExtra extends Extra
