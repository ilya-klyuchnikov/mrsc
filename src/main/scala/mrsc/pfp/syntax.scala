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

  def applySubst(t: Term, s: Subst): Term =
    null

  // can this subterm be extracted
  def isFreeSubTerm(t: Term, depth: Int): Boolean = t match {
    case BVar(i)       => i < depth
    case GVar(_)       => true
    case FVar(_)       => true
    case Abs(t1)       => isFreeSubTerm(t1, depth + 1)
    case App(t1, t2)   => isFreeSubTerm(t1, depth) && isFreeSubTerm(t2, depth)
    case Let(t1, t2)   => isFreeSubTerm(t1, depth) && isFreeSubTerm(t2, depth + 1)
    case Fix(t1)       => isFreeSubTerm(t1, depth)
    case Case(sel, bs) => isFreeSubTerm(sel, depth) && bs.forall(b => isFreeSubTerm(b._2, depth + 1))
    case Ctr(n, fs)    => fs.forall(f => isFreeSubTerm(f._2, depth))
    case DeCtr(t, f)   => isFreeSubTerm(t, depth)
  }

  def findSubst(from: Term, to: Term, depth: Int = 0): Option[Subst] = (from, to) match {
    case _ if from == to =>
      Some(Map())
    case _ if isVar(from) && isFreeSubTerm(to, depth) =>
      Some(Map(from -> to))
    case (Abs(t1), Abs(t2)) =>
      findSubst(t1, t2, depth + 1)
    case (App(h1, t1), App(h2, t2)) =>
      val s1 = findSubst(h1, h2, depth)
      val s2 = findSubst(t1, t2, depth)
      mergeOptSubst(s1, s2)
    case (Let(v1, t1), Let(v2, t2)) =>
      val s1 = findSubst(v1, v2, depth)
      val s2 = findSubst(t1, t2, depth + 1)
      mergeOptSubst(s1, s2)
    case (Fix(t1), Fix(t2)) =>
      findSubst(t1, t2, depth)
    case (Case(sel1, bs1), Case(sel2, bs2)) =>
      if (bs1.map(_._1) == bs2.map(_._1)) {
        var sub = findSubst(sel1, sel2, depth)
        for (((_, t1), (_, t2)) <- bs1.zip(bs2)) {
          val sub1 = findSubst(t1, t2, depth + 1)
          sub = mergeOptSubst(sub, sub1)
        }
        sub
      } else {
        None
      }
    case (Ctr(n1, fs1), Ctr(n2, fs2)) if n1 == n2 =>
      var sub: Option[Subst] = Some(Map())
      for (((_, t1), (_, t2)) <- fs1.zip(fs2)) {
        val sub1 = findSubst(t1, t2, depth)
        sub = mergeOptSubst(sub, sub1)
      }
      sub
    case _ => None
  }

  // can it be a variable we can abstract over?
  def isVar(t: Term): Boolean = t match {
    case FVar(_) => true
    case DeCtr(_, _) => true
    case _       => false
  }

  private def mergeOptSubst(s1: Option[Subst], s2: Option[Subst]): Option[Subst] =
    for {
      subst1 <- s1
      subst2 <- s2
      merged <- mergeSubst(subst1, subst2)
    } yield merged

  private def mergeSubst(sub1: Subst, sub2: Subst): Option[Subst] = {
    val merged1 = sub1 ++ sub2
    val merged2 = sub2 ++ sub1
    if (merged1 == merged2)
      Some(merged1)
    else
      None
  }
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
