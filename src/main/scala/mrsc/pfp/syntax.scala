package mrsc.pfp

sealed trait Term
// Nameless bound variable
case class BVar(i: Int) extends Term
// Named free variable
case class FVar(n: String) extends Term
// Named global variable from the global context
case class GVar(n: String) extends Term

case class Abs(t: Term) extends Term
case class App(t1: Term, t2: Term) extends Term
case class Let(l: String, t1: Term, t2: Term) extends Term
case class Fix(t: Term) extends Term

case class Ctr(tag: String, fields: List[Field]) extends Term
case class DeCtr(term: Term, field: String) extends Term
// inside the branch the destructured var is referenced
// by index 0
case class Case(sel: Term, branches: List[Branch]) extends Term

object Syntax {
  // substitute t for 0-var
  def termSubstTop(s: Term, t: Term): Term =
    null

  def findSubst(from: Term, to: Term): Option[Subst] =
    None

  def applySubst(t: Term, s: Subst): Term =
    null
}
