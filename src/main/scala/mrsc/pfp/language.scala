package mrsc.pfp

import mrsc.core._

trait PFPSyntax {
  def subst(c: Term, sub: Subst): Term
  def findSubst(from: Term, to: Term): Option[Subst]
  def rebuildings(c: Term): List[Rebuilding]
  def trivialRb(c: Term)(rb: Rebuilding) = (rb.sub.values.toSet + rb.t) exists { subclass.equiv(c, _) }
  def size(c: Term): Int
  val subclass: PartialOrdering[Term]
}

trait PFPSemantics {
  def driveStep(c: Term): MStep
}

trait Residuation {
  def residuate(graph: TGraph[Term, Label]): Term
}

trait MutualGens extends PFPSyntax {
  def mutualGens(c1: Term, c2: Term): List[Rebuilding] = {
    val nonTrivialRbs = rebuildings(c1) filterNot trivialRb(c1)
    nonTrivialRbs filter { rb => subclass.gteq(rb.t, c2) }
  }
}

trait MSG extends MutualGens {
  def msg(c1: Term, c2: Term): Option[Rebuilding] = {
    val mutual = mutualGens(c1, c2)
    mutual find { rb => mutual forall { other => subclass.lteq(rb.t, other.t) } }
  }
}
