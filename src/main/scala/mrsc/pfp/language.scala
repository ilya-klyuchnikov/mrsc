package mrsc.pfp

import mrsc.core._

trait PFPSyntax {
  def subst(c: Term, sub: Subst): Term
  def findSubst(from: Term, to: Term): Option[Subst]
  def rebuildings(c: MetaTerm): List[Rebuilding]
  def trivialRb(c: MetaTerm)(rb: Rebuilding) = (rb.sub.values.toSet + rb.t) exists { subclass.equiv(c, _) }
  def size(c: Term): Int
  val subclass: PartialOrdering[MetaTerm]
}

trait PFPSemantics {
  def driveStep(c: MetaTerm): MStep
}

trait Residuation {
  def residuate(graph: TGraph[Term, Label]): Term
}

trait MutualGens extends PFPSyntax {
  def mutualGens(c1: MetaTerm, c2: MetaTerm): List[Rebuilding] = {
    val nonTrivialRbs = rebuildings(c1) filterNot trivialRb(c1)
    nonTrivialRbs filter { rb => subclass.gteq(rb.t, c2) }
  }
}

trait MSG extends MutualGens {
  def msg(c1: MetaTerm, c2: MetaTerm): Option[Rebuilding] = {
    val mutual = mutualGens(c1, c2)
    mutual find { rb => mutual forall { other => subclass.lteq(rb.t, other.t) } }
  }
}
