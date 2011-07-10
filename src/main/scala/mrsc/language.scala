package mrsc

/*! # Means for specifying languages and ordering on language expressions.
 */

trait PreSyntax[C] {
  def instance: PartialOrdering[C]
  def rebuildings(c: C): List[C]
}

trait Syntax[C] extends PreSyntax[C] {
  def instance: PartialOrdering[C]
  def subst(c: C, sub: Subst[C]): C
  def rawRebuildings(c: C): List[Rebuilding[C]]
  def translate(rebuilding: Rebuilding[C]): C
  def rebuildings(c: C): List[C] = 
    rawRebuildings(c) map translate
  def findSubst(from: C, to: C): Option[Subst[C]]
  def size(c: C): Int
}

/*! # `MetaEvaluator` is an explosive mixture of 
 operational semantics and denotational semantics.
 Also it is a mix of small-step semantics and big-step semantics.
 */
trait OperationalSemantics[C] {
  def drive(c: C): DriveStep[C]
  def isDrivable(c: C): Boolean
}

// There are n rules. Variants should return a list of length n
trait RewriteSemantics[C] {
  def drive(c: C): List[Option[C]]
}

trait Residuation[C] {
  def residuate(graph: Graph[C, DriveInfo[C], _]): C
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