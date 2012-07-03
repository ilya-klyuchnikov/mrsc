package mrsc.pfp

// These operations are inspired by code from the book 
// "Types and Programming Languages"
object Syntax {

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
      case Fix(t1)      => Fix(walk(c, t1))
      case Case(t, bs)  => Case(walk(c, t), bs.map { case (ptr, ti) => (ptr, walk(c + ptr.args.size, ti)) })
      case Ctr(n, args) => Ctr(n, args.map(walk(c, _)))
    }
    walk(0, t)
  }

  def applySubst(t: Term, s: Subst): Term = {
    def walk(c: Int, t: Term): Term = t match {
      case v: BVar                       => v
      case v: FVar if s.get(v).isDefined => termShift(c, s(v))
      case v: FVar                       => v
      case v: GVar                       => v
      case Abs(t2)                       => Abs(walk(c + 1, t2))
      case App(t1, t2)                   => App(walk(c, t1), walk(c, t2))
      case Let(t1, t2)                   => Let(walk(c, t1), walk(c + 1, t2))
      case Fix(t1)                       => Fix(walk(c, t1))
      case Case(t, bs)                   => Case(walk(c, t), bs.map { case (ptr, ti) => (ptr, walk(c + ptr.args.size, ti)) })
      case Ctr(n, fs)                    => Ctr(n, fs.map(walk(c, _)))
    }
    walk(0, t)
  }

  // shifts unbound bvars by d
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
    case Fix(t1)       => isFreeSubTerm(t1, depth)
    case Case(sel, bs) => isFreeSubTerm(sel, depth) && bs.forall(b => isFreeSubTerm(b._2, depth + 1))
    case Ctr(n, fs)    => fs.forall(isFreeSubTerm(_, depth))
  }

  def findSubst(from: Term, to: Term): Option[Subst] = (from, to) match {
    case _ if from == to =>
      Some(Map())
    case (fv: FVar, _) if isFreeSubTerm(to, 0) =>
      Some(Map(fv -> to))
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
      for ((t1, t2) <- fs1.zip(fs2)) {
        val sub1 = findSubst(t1, t2)
        sub = mergeOptSubst(sub, sub1)
      }
      sub
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

}

trait VarGen {
  var freeVar: Int = 10
  def nextVar(x: Any = ()): FVar = {
    freeVar += 1
    FVar(freeVar)
  }
}

