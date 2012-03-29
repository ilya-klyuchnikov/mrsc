package mrsc.pfp

// Simple higher-order pure functional language.
// The main purpose of this representation is to move
// to use nameless variables as much as possible
// in order to make easy normalization by super-
// compilation and two-level supercompilation.
sealed trait Term
// Nameless bound variable
case class BVar(i: Int) extends Term {
  override def toString = i.toString
}
// Named free variable
case class FVar(n: String) extends Term {
  override def toString = "<" + n + ">"
}
// Named global variable from the global context
case class GVar(n: String) extends Term {
  override def toString = n
}
// Lambda abstraction
case class Abs(t: Term) extends Term {
  override def toString = "(\\" + t + ")"
}
// Application
case class App(t1: Term, t2: Term) extends Term {
  override def toString = "(" + t1 + " " + t2 + ")"
}
// Simple let-expression
case class Let(v: Term, in: Term) extends Term
// Fix point combinator.
case class Fix(t: Term) extends Term
// Constructor with explicit labeling of its parts.
case class Ctr(tag: String, fields: List[Field]) extends Term {
  override def toString = tag + fields.map(f => f._1 + ": " + f._2).mkString("[", ", ", "]")
}
// Extracting part of a term. Usually a result of case-expression.
case class DeCtr(term: Term, field: String) extends Term {
  override def toString = term + "." + field
}
// Inside the branch the selector is referenced.
// Its parts are referenced by 0.head, 0.tail, ...
case class Case(sel: Term, branches: List[Branch]) extends Term {
  override def toString = "case " + sel + " of " + branches.map(b => b._1 + "-> " + b._2).mkString("{", "; ", "}")
}

// These operations are inspired by code from the book 
// "Types and Programming Languages"
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

  // substitute s for 0-var in t
  def termSubstTop(s: Term, t: Term): Term =
    termShift(-1, termSubst(0, termShift(1, s), t))

  // TODO
  def applySubst(t: Term, s: Subst): Term =
    null

  // can this subterm be extracted
  def isFreeSubTerm(t: Term, depth: Int = 0): Boolean = t match {
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

  // seems that we do not need depth here!
  def findSubst(from: Term, to: Term): Option[Subst] = (from, to) match {
    case _ if from == to =>
      Some(Map())
    case _ if isVar(from) && isFreeSubTerm(to, 0) =>
      Some(Map(from -> to))
    case (Abs(t1), Abs(t2)) =>
      findSubst(t1, t2)
    case (App(h1, t1), App(h2, t2)) =>
      val s1 = findSubst(h1, h2)
      val s2 = findSubst(t1, t2)
      mergeOptSubst(s1, s2)
    case (Let(v1, t1), Let(v2, t2)) =>
      val s1 = findSubst(v1, v2)
      val s2 = findSubst(t1, t2)
      mergeOptSubst(s1, s2)
    case (Fix(t1), Fix(t2)) =>
      findSubst(t1, t2)
    case (Case(sel1, bs1), Case(sel2, bs2)) =>
      if (bs1.map(_._1) == bs2.map(_._1)) {
        var sub = findSubst(sel1, sel2)
        for (((_, t1), (_, t2)) <- bs1.zip(bs2)) {
          val sub1 = findSubst(t1, t2)
          sub = mergeOptSubst(sub, sub1)
        }
        sub
      } else {
        None
      }
    case (Ctr(n1, fs1), Ctr(n2, fs2)) if n1 == n2 =>
      var sub: Option[Subst] = Some(Map())
      for (((_, t1), (_, t2)) <- fs1.zip(fs2)) {
        val sub1 = findSubst(t1, t2)
        sub = mergeOptSubst(sub, sub1)
      }
      sub
    case _ => None
  }

  // can it be a variable we can abstract over?
  def isVar(t: Term): Boolean = t match {
    case FVar(_)     => true
    case DeCtr(_, _) => true
    case _           => false
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

  def renaming(t1: Term, t2: Term): Boolean = {
    val subst1 = findSubst(t1, t2)
    val subst2 = findSubst(t2, t2)
    (subst1, subst2) match {
      case (Some(s1), Some(s2)) =>
        true
      //s1.values.toSet == s2.keySet
      case _ => false
    }
  }

  // replace every occurrence of t1 in t by t2
  // t1 and t2 should be free terms
  def replace(t: Term, t1: Term, t2: Term): Term = {
    require(isFreeSubTerm(t1, 0))
    require(isFreeSubTerm(t2, 0))
    t match {
      case _ if t == t1 =>
        t2
      case Abs(t3) =>
        Abs(replace(t3, t1, t2))
      case App(a1, a2) =>
        App(replace(a1, t1, t2), replace(a2, t1, t2))
      case Let(l1, l2) =>
        Let(replace(l1, t1, t2), replace(l2, t1, t2))
      case Fix(f) =>
        Fix(replace(f, t1, t2))
      case Ctr(n, fs) =>
        val fs1 = fs.map { case (li, ti) => (li, replace(ti, t1, t2)) }
        Ctr(n, fs1)
      case DeCtr(c, f) =>
        DeCtr(replace(c, t1, t2), f)
      case Case(sel, bs) =>
        val sel1 = replace(sel, t1, t2)
        val bs1 = bs.map { case (li, ti) => (li, replace(ti, t1, t2)) }
        Case(sel1, bs1)
      case _ =>
        t
    }
  }
}
