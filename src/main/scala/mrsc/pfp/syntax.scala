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
case class Let(v: Term, in: Term) extends Term
case class Fix(t: Term) extends Term

case class Ctr(tag: String, fields: List[Field]) extends Term
case class DeCtr(term: Term, field: String) extends Term
// inside the branch the destructured var is referenced
// by index 0
case class Case(sel: Term, branches: List[Branch]) extends Term

object Syntax {
  private def tmMap(onVar: (Int, BVar) => Term, c: Int, t: Term): Term = {
    def walk(c: Int, t: Term): Term = t match {
      case v: BVar     => onVar(c, v)
      case v: FVar     => v
      case v: GVar     => v
      case Abs(t2)     => Abs(walk(c + 1, t2))
      case App(t1, t2) => App(walk(c, t1), walk(c, t2))
      case Let(t1, t2) => Let(walk(c, t1), walk(c + 1, t2))
      case Fix(t1)     => Fix(walk(c, t1))
      case Case(t, bs) => Case(walk(c, t), bs.map { case (li, ti) => (li, walk(c + 1, ti)) })
      case Ctr(n, fs)  => Ctr(n, fs.map { case (tagi, ti) => (tagi, walk(c, ti)) })
      case DeCtr(t, f) => DeCtr(walk(c, t), f)
    }
    walk(c, t)
  }

  private def termShiftAbove(d: Int, c: Int, t: Term): Term = {
    val f = { (c: Int, v: BVar) =>
      if (v.i >= c) BVar(v.i + d) else BVar(v.i)
    }
    tmMap(f, c, t)
  }

  private def termShift(d: Int, t: Term): Term =
    termShiftAbove(d, 0, t)

  private def termSubst(j: Int, s: Term, t: Term): Term = {
    val onVar = { (c: Int, v: BVar) =>
      if (v.i == c) termShift(c, s) else v
    }
    tmMap(onVar, j, t)
  }

  // substitute t for 0-var
  def termSubstTop(s: Term, t: Term): Term =
    termShift(-1, termSubst(0, termShift(1, s), t))

  def findSubst(from: Term, to: Term): Option[Subst] =
    None

  def applySubst(t: Term, s: Subst): Term =
    null
}

case class Context(l: List[String] = List()) {
  val length: Int = l.length
  def addName(s: String): Context = Context(s :: l)
  def isNameBound(s: String): Boolean = l.exists { _ == s }
  def name2index(s: String): Int = l.indexWhere { _ == s } match {
    case -1 => throw new Exception("identifier " + s + " is unbound")
    case i  => i
  }
}
