package mrsc.pfp

import mrsc.core._

trait PFPSyntax[C] {
  def subst(c: C, sub: Subst[C]): C
  def findSubst(from: C, to: C): Option[Subst[C]]
  def rawRebuildings(c: C): List[RawRebuilding[C]]
  def translate(rebuilding: RawRebuilding[C]): C
  def trivialRb(c: C)(rb: RawRebuilding[C]) =
    (rb._2.values.toSet + rb._1) exists { subclass.equiv(c, _) }
  def rebuildings(c: C): List[C] =
    rawRebuildings(c) filterNot trivialRb(c) map translate
  def size(c: C): Int
  val subclass: PartialOrdering[C]
}

trait PFPSemantics[C] {
  def driveStep(c: C): DriveStep[C]
}

trait Residuation[C] {
  def residuate(graph: TGraph[C, DriveInfo[C]]): C
}

trait MutualGens[C] extends PFPSyntax[C] {
  def mutualGens(c1: C, c2: C): List[RawRebuilding[C]] = {
    val nonTrivialRbs = rawRebuildings(c1) filterNot trivialRb(c1)
    nonTrivialRbs filter { rb => subclass.gteq(rb._1, c2) }
  }
}

trait MSG[C] extends MutualGens[C] {
  def msg(c1: C, c2: C): Option[RawRebuilding[C]] = {
    val mutual = mutualGens(c1, c2)
    mutual find { rb => mutual forall { other => subclass.lteq(rb._1, other._1) } }
  }
}

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