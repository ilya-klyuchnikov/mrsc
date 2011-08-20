package mrsc.pfp

import mrsc.core._

trait PFPSyntax[C] extends EquivAndInstanceOf[C] {
  def subst(c: C, sub: Subst[C]): C
  def findSubst(from: C, to: C): Option[Subst[C]]
  def rawRebuildings(c: C): List[RawRebuilding[C]]
  def translate(rebuilding: RawRebuilding[C]): C
  def trivialRb(c: C)(rb: RawRebuilding[C]) =
    equiv(c, rb._1) || rb._2.values.exists(equiv(c, _))
  def rebuildings(c: C): List[C] =
    rawRebuildings(c) filterNot trivialRb(c) map translate
  def size(c: C): Int
}

trait OperationalSemantics[C] {
  def driveStep(c: C): DriveStep[C]
}

trait Residuation[C] {
  def residuate(graph: TGraph[C, DriveInfo[C], _]): C
}

trait MSG[C] extends PFPSyntax[C] {

  def msg(c1: C, c2: C): Option[RawRebuilding[C]] = {
    val nonTrivialRbs = rawRebuildings(c1) filterNot trivialRb(c1)
    val sharedRbs = nonTrivialRbs filter { rb => instanceOf(c2, rb._1) }
    sharedRbs find { rb => sharedRbs forall { other => instanceOf(rb._1, other._1) } }
  }

}