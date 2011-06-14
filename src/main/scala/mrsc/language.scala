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

sealed trait EvalStep[+C]
case class Transient1[C](next: C) extends EvalStep[C]
case object Stop1 extends EvalStep[Nothing]
case class Decomposition1[C](compose: List[C] => C, parts: List[C]) extends EvalStep[C]
case class Variants1[C](cases: List[(Contraction[C], C)]) extends EvalStep[C]

/*! # `MetaEvaluator` is an explosive mixture of 
 operational semantics and denotational semantics.
 Also it is a mix of small-step semantics and big-step semantics.
 */
trait MetaEvaluator[C] {
  def eval(c: C): EvalStep[C]
  def isReducible(c: C): Boolean
}

trait Termination[C] {
  def embedding: PartialOrdering[C]
}

trait Residuator[C, R] {
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