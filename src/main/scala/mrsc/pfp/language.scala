package mrsc.pfp

import mrsc.core._

trait Syntax[C] {
  def instance: PartialOrdering[C]
  def subst(c: C, sub: Subst[C]): C
  def rawRebuildings(c: C): List[Rebuilding[C]]
  def translate(rebuilding: Rebuilding[C]): C
  def trivialRb(c: C)(rb: Rebuilding[C]) =
    instance.equiv(c, rb._1) || rb._2.values.exists(instance.equiv(c, _))
  def rebuildings(c: C): List[C] =
    rawRebuildings(c) filterNot trivialRb(c) map translate
  def findSubst(from: C, to: C): Option[Subst[C]]
  def size(c: C): Int
}

trait OperationalSemantics[C] {
  def drive(c: C): DriveStep[C]
  def isDrivable(c: C): Boolean
}

trait Residuation[C] {
  def residuate(graph: Graph[C, DriveInfo[C], _]): C
}

trait MSG[C] extends Syntax[C] {

  def msg(c1: C, c2: C): Option[Rebuilding[C]] = {
    val nonTrivialRbs = rawRebuildings(c1) filterNot trivialRb(c1)
    val sharedRbs = nonTrivialRbs filter { rb => instance.lteq(rb._1, c2) }
    sharedRbs find { rb => sharedRbs forall { other => instance.lteq(other._1, rb._1) } }
  }

}