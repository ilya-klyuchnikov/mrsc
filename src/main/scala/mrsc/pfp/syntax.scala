package mrsc.pfp

// These operations are inspired by code from the book 
// "Types and Programming Languages"
// 
object Syntax {

  // Given a term t and a function onVar, 
  // the result of tmMap onVar t is a term of the same shape as t 
  // in which every *bound* variable has been replaced by the result of calling onVar on that variable.
  // c = initial "context depth"
  // onVar(c, v) - here c is current context depth
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

  // Def 6.2.1 
  // The d-place shift of a term t above cutoff c.
  // ↑dc (t)
  def termShiftAbove(d: Int, c: Int, t: Term): Term = {
    val f = { (c: Int, v: BVar) =>
      if (v.i >= c) BVar(v.i + d) else BVar(v.i)
    }
    tmMap(f, c, t)
  }

  // The top level shift ↑d (t)
  def termShift(d: Int, t: Term): Term =
    termShiftAbove(d, 0, t)

  // right now termSubst is called only with j = 0.
  // Be careful with other cases!!
  // What to do depends on what you mean! (See note in google docs.)
  private def termSubst(j: Int, s: Term, t: Term): Term = {
    val onVar = { (c: Int, v: BVar) =>
      if (v.i == c) termShift(c, s) else v
    }
    tmMap(onVar, j, t)
  }

  // substitute s for 0-var in t
  def termSubstTop(s: Term, t: Term): Term =
    termShift(-1, termSubst(0, termShift(1, s), t))

  def applySubst(t: Term, s: Subst): Term = {
    def walk(c: Int, t: Term): Term = t match {
      case v @ BVar(i) if s.get(BVar(i + c)).isDefined =>
        Syntax.termShift(c, s(BVar(i + c)))
      case v: BVar =>
        v
      case v: FVar if s.get(v).isDefined =>
        Syntax.termShift(c, s(v))
      case v: FVar =>
        v
      case v: GVar =>
        v
      case Abs(t2) =>
        Abs(walk(c + 1, t2))
      case App(t1, t2) =>
        App(walk(c, t1), walk(c, t2))
      case Let(t1, t2) =>
        Let(walk(c, t1), walk(c + 1, t2))
      case Fix(t1) =>
        Fix(walk(c, t1))
      case Case(t, bs) =>
        Case(walk(c, t), bs.map { case (li, ti) => (li, walk(c + 1, ti)) })
      case Ctr(n, fs) =>
        Ctr(n, fs.map { case (tagi, ti) => (tagi, walk(c, ti)) })
      case DeCtr(t, f) =>
        DeCtr(walk(c, t), f)
    }
    walk(0, t)
  }

  // can this subterm be extracted?
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

  // Replace every occurrence of t1 in t by t2
  // We assume that both t1 and t2 are free subterms 
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

  def freeVars(t: Term): List[FVar] = t match {
    case fv @ FVar(_)  => List(fv)
    case BVar(_)       => List()
    case GVar(_)       => List()
    case Abs(t1)       => freeVars(t1)
    case App(t1, t2)   => List(freeVars(t1), freeVars(t2)).flatten.distinct
    case Let(t1, t2)   => List(freeVars(t1), freeVars(t2)).flatten.distinct
    case Fix(t1)       => freeVars(t1)
    case Ctr(_, args)  => args.map(_._2).map(freeVars).flatten.distinct
    case DeCtr(t1, _)  => freeVars(t1)
    case Case(sel, bs) => (freeVars(sel) :: bs.map(_._2).map(freeVars)).flatten.distinct
  }

}
