package mrsc.pfp

sealed trait Term
// Nameless bound variable
case class BVar(i: Int, cl: Int) extends Term
// Named free variable
case class FVar(n: String) extends Term

case class TmAbs(v: String, t: Term) extends Term
case class TmApp(t1: Term, t2: Term) extends Term
case class TmLet(l: String, t1: Term, t2: Term) extends Term
case class TmFix(t: Term) extends Term

case class Ctr(tag: String, fields: List[Field]) extends Term
case class DeCtr(term: Term, field: String) extends Term
case class TmCase(sel: Term, branches: List[Branch]) extends Term