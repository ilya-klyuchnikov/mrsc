package mrsc

/*! # Means for specifying languages and ordering on language expressions.
 */

trait Syntax[C] {
  def instance: PartialOrdering[C]
  def subst(c: C, sub: Subst[C]): C
  def rebuildings(c: C): List[Rebuilding[C]]
  def rebuilding2Configuration(rebuilding: Rebuilding[C]): C
  def findSubst(from: C, to: C): Option[Subst[C]]
}

/*! # `MetaEvaluator` is an explosive mixture of 
 operational semantics and denotational semantics.
 Also it is a mix of small-step semantics and big-step semantics.
 */
trait Semantics[C] {
  def drive(c: C): DriveStep[C]
  def isDrivable(c: C): Boolean
}

trait Residuation[C, R] {
  def residuate(graph: Graph[R, DriveInfo[R], _]): R
}

trait SimplePartialOrdering[T] extends PartialOrdering[T] {
  override def tryCompare(x: T, y: T): Option[Int] = (lteq(x, y), lteq(y, x)) match {
    case (false, false) =>
      None
    case (false, true) =>
      Some(1)
    case (true, false) =>
      Some(-1)
    case (true, true) =>
      Some(0)
  }
}