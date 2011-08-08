package mrsc.pfp

import mrsc.core._

/*! # Means for specifying languages and ordering on language expressions.
 */

trait Syntax[C] {
  def instance: PartialOrdering[C]
  def subst(c: C, sub: Subst[C]): C
  def rawRebuildings(c: C): List[Rebuilding[C]]
  def translate(rebuilding: Rebuilding[C]): C
  def rebuildings(c: C): List[C] = rawRebuildings(c) map translate
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

trait NaiveMSG[C] extends Syntax[C] {

  def lt(e1: C, e2: C): Boolean =
    if (size(e1) < size(e2)) {
      true
    } else if (size(e1) > size(e2)) {
      false
    } else {
      instance.lt(e1, e2)
    }

  def msg(c: C, c2: C): Option[Rebuilding[C]] = {

    val idRebuilding: Rebuilding[C] = (c, Map[Name, C]())
    val allRebuildings: List[Rebuilding[C]] = idRebuilding :: rawRebuildings(c)
    val sharedRebuildings = allRebuildings filter { case (c1, _) => instance.lteq(c1, c2) }

    val msgs = sharedRebuildings filter {
      case rb @ (c1, _) => (sharedRebuildings.remove(_ == rb)) forall {
        case (c3, _) => instance.lteq(c3, c1)
      }
    }

    val msgs1 = msgs filter { case (c1, rb) => lt(c1, c) && rb.values.forall { lt(_, c) } }
    msgs1.headOption
  }

}