package mrsc.pfp

// PART 1. AST
sealed trait MetaTerm
case class Rebuilding(t: Term, sub: Subst) extends MetaTerm

sealed trait Term extends MetaTerm {
  def size: Int
}
case class BVar(i: Int) extends Term {
  override lazy val size = 1
}
case class FVar(i: Int) extends Term {
  override lazy val size = 1
}
case class GVar(n: String) extends Term {
  override lazy val size = 1
}
case class Abs(t: Term) extends Term {
  override lazy val size = 1 + t.size
}
case class App(t1: Term, t2: Term) extends Term {
  override lazy val size = t1.size + t2.size
}
// Simple let-expression. `v` is represented by `BVar(0)` in `in`.
case class Let(v: Term, in: Term) extends Term {
  override lazy val size = 1 + v.size + in.size
}
// Term itself is represented as `BVar(0)` in `t`.
// In terms of TAPL we use only Fix(Abs(_)) combination.
// Let(Fix(_), e) is a letrec
case class Fix(t: Term) extends Term {
  override lazy val size = 1 + t.size
}
case class Ctr(name: String, args: List[Term]) extends Term {
  override lazy val size = 1 + args.map(_.size).sum
}
case class Case(sel: Term, branches: List[Branch]) extends Term {
  override lazy val size = sel.size + branches.map{b => b._2.size}.sum
}
case class Ptr(name: String, args: List[String])

// Labels or actions of our LTS.
// We put it on graph edges.
sealed trait Label
case object TransientLabel extends Label {
  override def toString = "->"
}
case object UnfoldLabel extends Label {
  override def toString = "->*"
}
case class CaseBranchLabel(sel: Term, ptr: Ptr, alt: Ctr) extends Label {
  override def toString = sel + " = " + alt
}
case class DecomposeLabel(compose: List[Term] => Term) extends Label {
  override def toString = ""
}

// PART 2. Syntax operations
/**
 * Utility to work with nameless syntax (via indexes).
 * This implementation is based on the book "Types and Programming Languages".
 * Residuator utilizes the facility on nameless syntax to work with bound
 * variables directly.
 */
object NamelessSyntax {

  // Given a term t and a function onVar,
  // the result of tmMap onVar t is a term of the same shape as t
  // in which every *bound* variable has been replaced by the result of calling onVar on that variable.
  // c = initial "context depth"
  // onVar(c, v) - here c is current context depth
  private def tmMap(onVar: (Int, BVar) => Term, t: Term): Term = {
    // c - current context depth
    def walk(c: Int, t: Term): Term = t match {
      case v: BVar      => onVar(c, v)
      case v: FVar      => v
      case v: GVar      => v
      case Abs(t2)      => Abs(walk(c + 1, t2))
      case App(t1, t2)  => App(walk(c, t1), walk(c, t2))
      case Let(t1, t2)  => Let(walk(c, t1), walk(c + 1, t2))
      case Fix(t1)      => Fix(walk(c + 1, t1))
      case Case(t, bs)  => Case(walk(c, t), bs.map { case (ptr, ti) => (ptr, walk(c + ptr.args.size, ti)) })
      case Ctr(n, args) => Ctr(n, args.map(walk(c, _)))
    }
    walk(0, t)
  }

  // in general case the domain of s may be bound var
  // (for example, in residuator)
  def applySubst(t: Term, s: Subst): Term = {
    def walk(c: Int, t: Term): Term = t match {
      case v: BVar                       => v
      case v: FVar if s.get(v).isDefined => termShift(c, s(v))
      case v: FVar                       => v
      case v: GVar                       => v
      case Abs(t2)                       => Abs(walk(c + 1, t2))
      case App(t1, t2)                   => App(walk(c, t1), walk(c, t2))
      case Let(t1, t2)                   => Let(walk(c, t1), walk(c + 1, t2))
      case Fix(t1)                       => Fix(walk(c + 1, t1))
      case Case(t, bs)                   => Case(walk(c, t), bs.map { case (ptr, ti) => (ptr, walk(c + ptr.args.size, ti)) })
      case Ctr(n, fs)                    => Ctr(n, fs.map(walk(c, _)))
    }
    walk(0, t)
  }

  // shifts unbound bvars by d
  // unbound bvars may appear during beta reduction (termSubstTop)
  def termShift(d: Int, t: Term): Term = {
    val f = { (c: Int, v: BVar) => if (v.i >= c) BVar(v.i + d) else BVar(v.i) }
    tmMap(f, t)
  }

  // replaces BVar(0) in t by s
  private def termSubst(s: Term, t: Term): Term = {
    val onVar = { (c: Int, v: BVar) => if (v.i == c) termShift(c, s) else v }
    tmMap(onVar, t)
  }

  // substitute s for 0-var in t
  // used for reductions
  def termSubstTop(s: Term, t: Term): Term =
    termShift(-1, termSubst(termShift(1, s), t))

  // can this subterm be extracted?
  def isFreeSubTerm(t: Term, depth: Int = 0): Boolean = t match {
    case BVar(i)       => i < depth
    case GVar(_)       => true
    case FVar(_)       => true
    case Abs(t1)       => isFreeSubTerm(t1, depth + 1)
    case App(t1, t2)   => isFreeSubTerm(t1, depth) && isFreeSubTerm(t2, depth)
    case Let(t1, t2)   => isFreeSubTerm(t1, depth) && isFreeSubTerm(t2, depth + 1)
    case Fix(t1)       => isFreeSubTerm(t1, depth + 1)
    case Case(sel, bs) => isFreeSubTerm(sel, depth) && bs.forall{ case (ptr, ti) => isFreeSubTerm(ti, depth + ptr.args.size)}
    case Ctr(n, fs)    => fs.forall(isFreeSubTerm(_, depth))
  }

  def findSubst(from: Term, to: Term): Option[Subst] =
    for (sub <- findSubst0(from, to))
    yield sub.filter { case (k, v) => k != v }

  def findSubst0(from: Term, to: Term): Option[Subst] = (from, to) match {
    case (fv: FVar, _) if isFreeSubTerm(to, 0) =>
      Some(Map(fv -> to))
    case (Abs(t1), Abs(t2)) =>
      findSubst0(t1, t2)
    case (App(h1, t1), App(h2, t2)) =>
      val s1 = findSubst0(h1, h2)
      val s2 = findSubst0(t1, t2)
      mergeOptSubst(s1, s2)
    case (Let(v1, t1), Let(v2, t2)) =>
      val s1 = findSubst0(v1, v2)
      val s2 = findSubst0(t1, t2)
      mergeOptSubst(s1, s2)
    case (Fix(t1), Fix(t2)) =>
      findSubst0(t1, t2)
    case (Case(sel1, bs1), Case(sel2, bs2)) =>
      if (bs1.map(_._1) == bs2.map(_._1)) {
        var sub = findSubst0(sel1, sel2)
        for (((_, t1), (_, t2)) <- bs1.zip(bs2)) {
          val sub1 = findSubst0(t1, t2)
          sub = mergeOptSubst(sub, sub1)
        }
        sub
      } else {
        None
      }
    case (Ctr(n1, fs1), Ctr(n2, fs2)) if n1 == n2 =>
      var sub: Option[Subst] = Some(Map())
      for ((t1, t2) <- fs1.zip(fs2)) {
        val sub1 = findSubst0(t1, t2)
        sub = mergeOptSubst(sub, sub1)
      }
      sub
    case (BVar(i), BVar(j)) if i == j =>
      Some(Map())
    case (GVar(i), GVar(j)) if i == j =>
      Some(Map())
    case _ => None
  }

  private def mergeOptSubst(s1: Option[Subst], s2: Option[Subst]): Option[Subst] =
    for (subst1 <- s1; subst2 <- s2; merged <- mergeSubst(subst1, subst2))
    yield merged

  private def mergeSubst(sub1: Subst, sub2: Subst): Option[Subst] = {
    val merged1 = sub1 ++ sub2
    val merged2 = sub2 ++ sub1
    if (merged1 == merged2)
      Some(merged1)
    else
      None
  }

  // brute-force testing for renaming
  def renaming(t1: Term, t2: Term): Boolean =
    findSubst(t1, t2).isDefined && findSubst(t2, t1).isDefined

  def freeVars(t: Term): List[FVar] = t match {
    case fv @ FVar(_)  => List(fv)
    case BVar(_)       => List()
    case GVar(_)       => List()
    case Abs(t1)       => freeVars(t1)
    case App(t1, t2)   => List(freeVars(t1), freeVars(t2)).flatten.distinct
    case Let(t1, t2)   => List(freeVars(t1), freeVars(t2)).flatten.distinct
    case Fix(t1)       => freeVars(t1)
    case Ctr(_, args)  => args.map(freeVars).flatten.distinct
    case Case(sel, bs) => (freeVars(sel) :: bs.map(_._2).map(freeVars)).flatten.distinct
  }

  // here we mean subclass in semantical sense (as subset)
  val subclass: PartialOrdering[MetaTerm] = new SimplePartialOrdering[MetaTerm] {
    override def lteq(t1: MetaTerm, t2: MetaTerm) = (t1, t2) match {
      case (t1: Term, t2: Term) => findSubst(t2, t1).isDefined
      case _                    => false
    }
  }

}
