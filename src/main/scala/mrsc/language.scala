package mrsc

/*! # Means for specifying languages.
 */

trait Syntax[C] {
  def instance: PartialOrdering[C]
  def subst(c: C, sub: Subst[C]): C
  def rebuildings(c: C): List[Rebuilding[C]]
  def rebuilding2Configuration(rebuilding: Rebuilding[C]): C
  def findSubst(from: C, to: C): Option[Subst[C]]
}

case class Contraction1[C](param: Name, pattern: C)
sealed trait EvalStep[+C]
case class Transient1[C](next: C) extends EvalStep[C]
case object Stop1 extends EvalStep[Nothing]
case class Decomposition1[C](parts: List[C], compose: List[C] => C) extends EvalStep[C]
case class Variants1[C](cases: List[(Contraction1[C], C)]) extends EvalStep[C]

/*! # `MetaEvaluator` is an explosive mixture of 
 operational semantics and denotational semantics.
 Also it is a mix of small-step semantics and big-step semantics.
 */
trait MetaEvaluator[C] {
  def eval(c: C): EvalStep[C]
}

trait Termination[C] {
  def embeddding: PartialOrdering[C]
}

trait Residuator[C, R] {
  def residuate(graph: Graph[R, SubStepInfo[R], _]): R
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