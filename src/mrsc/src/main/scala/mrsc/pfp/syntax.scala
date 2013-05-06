package mrsc.pfp

// PART 1. AST
sealed trait MetaTerm { self =>
  val ticks: Int
}
case class Rebuilding(t: Term, sub: Subst, ticks: Int = 0) extends MetaTerm

sealed trait Term extends MetaTerm {
  def size: Int
}
case class BVar(i: Int, ticks: Int = 0) extends Term {
  override lazy val size = 1
}
case class FVar(i: Int, ticks: Int = 0) extends Term {
  override lazy val size = 1
}
case class GVar(n: String, ticks: Int = 0) extends Term {
  override lazy val size = 1
}
case class Abs(t: Term, ticks: Int = 0) extends Term {
  override lazy val size = 1 + t.size
}
case class App(t1: Term, t2: Term, ticks: Int = 0) extends Term {
  override lazy val size = t1.size + t2.size
}
// Simple let-expression. `v` is represented by `BVar(0)` in `in`.
case class Let(v: Term, in: Term, ticks: Int = 0) extends Term {
  override lazy val size = 1 + v.size + in.size
}
// Term itself is represented as `BVar(0)` in `t`.
// In terms of TAPL we use only Fix(Abs(_)) combination.
// Let(Fix(_), e) is a letrec
case class Fix(t: Term, ticks: Int = 0) extends Term {
  override lazy val size = 1 + t.size
}
case class Ctr(name: String, args: List[Term], ticks: Int = 0) extends Term {
  override lazy val size = 1 + args.map(_.size).sum
}
case class Case(sel: Term, branches: List[Branch], ticks: Int = 0) extends Term {
  override lazy val size = sel.size + branches.map{b => b._2.size}.sum
}
case class Ptr(name: String, args: List[String])

object Ticks {
  def incrTicks[T <: MetaTerm](t: T, delta: Int): T = (t match {
    case t: Rebuilding => t.copy(ticks = t.ticks + delta)
    case t: BVar => t.copy(ticks = t.ticks + delta)
    case t: FVar => t.copy(ticks = t.ticks + delta)
    case t: GVar => t.copy(ticks = t.ticks + delta)
    case t: Abs => t.copy(ticks = t.ticks + delta)
    case t: App => t.copy(ticks = t.ticks + delta)
    case t: Let => t.copy(ticks = t.ticks + delta)
    case t: Fix => t.copy(ticks = t.ticks + delta)
    case t: Ctr => t.copy(ticks = t.ticks + delta)
    case t: Case => t.copy(ticks = t.ticks + delta)
  }).asInstanceOf[T]

  def zeroTicks[T <: MetaTerm](t: T): T = (t match {
    case t: Rebuilding => t.copy(ticks = 0)
    case t: BVar => t.copy(ticks = 0)
    case t: FVar => t.copy(ticks = 0)
    case t: GVar => t.copy(ticks = 0)
    case t: Abs => t.copy(ticks = 0)
    case t: App => t.copy(ticks = 0)
    case t: Let => t.copy(ticks = 0)
    case t: Fix => t.copy(ticks = 0)
    case t: Ctr => t.copy(ticks = 0)
    case t: Case => t.copy(ticks = 0)
  }).asInstanceOf[T]

  // tick improvement
  private def i(t1: Term, t2: Term): Boolean = (t1, t2) match {
    case (BVar(_, ticks1), BVar(_, ticks2)) =>
      ticks1 <= ticks2
    case (FVar(_, ticks1), FVar(_, ticks2)) =>
      ticks1 <= ticks2
    case (GVar(_, ticks1), GVar(_, ticks2)) =>
      ticks1 <= ticks2
    case (Abs(t1, ticks1), Abs(t2, ticks2)) =>
      ticks1 <= ticks2 && i(t1, t2)
    case (App(h1, t1,  ticks1), App(h2, t2, ticks2)) =>
      ticks1 <= ticks2 && i(h1, h2) && i(t1, t2)
    case (Let(e1, in1, ticks1), Let(e2, in2, ticks2)) =>
      ticks1 <= ticks2 && i(e1, e2) && i(in1, in2)
    case (Fix(e1, ticks1), Fix(e2, ticks2)) =>
      ticks1 <= ticks2 && i(e1, e2)
    case (Ctr(_, args1, ticks1), Ctr(_, args2, ticks2)) =>
      ticks1 <= ticks2 && (args1 zip args2).forall{case (e1, e2) => i(e1, e2)}
    case (Case(sel1, bs1, ticks1), Case(sel2, bs2, ticks2)) =>
      ticks1 <= ticks2 && i(sel1, sel2) && (bs1 zip bs2).forall{case ((_, e1), (_, e2)) => i(e1, e2)}
    case _ => false
  }

  def isImprovement(t1: Term, t2: Term) = i(t1, t2)

  def reset(t: Term): Term = (t match {
    case t: BVar => t.Z
    case t: FVar => t.Z
    case t: GVar => t.Z
    case Abs(e, _) => Abs(reset(e))
    case App(e1, e2, _) => App(reset(e1), reset(e2))
    case Let(e1, e2, _) => Let(reset(e1), reset(e2))
    case Fix(e1, _) => Fix(reset(e1))
    case Ctr(n, args, _) => Ctr(n, args.map(reset))
    case Case(sel, bs, _) => Case(reset(sel), bs.map {case (p, e) => (p, reset(e))})
  })

  implicit class TickOps(t: Term) {
    def I(delta: Int) = Ticks.incrTicks(t, delta)
    def Z = Ticks.zeroTicks(t)
  }
}

// There possible cases when we need multi-result rules and maybe back rules
object TicksNorm {
  import Ticks._
  import NamelessSyntax._
  // normalization rule
  type NRule = PartialFunction[Term, Term]
  type WRule = PartialFunction[(Int, Term), Term]

  val caseNorm: NRule = {
    case Case(sel, bs, ticks) if sel.ticks > 0 || ticks > 0 =>
      Case(sel.Z, bs.map {case (p, e) => (p, e.I(ticks + sel.ticks))})
  }

  // TODO: Generalize to arbitrary arity
  def letRecNorm1: NRule = {
    case Let(Fix(Abs(e1, aTicks), fTicks), app@App(BVar(0, _), e2, _), lTicks) if e1.ticks > 0 =>
      var rule: WRule =
        {case (c, app@App(BVar(v, _), e2, _)) if v == c + 1 => app.I(e1.ticks)}
      val e1Norm = rewrite(rule, e1.Z)
      Let(Fix(Abs(e1Norm, aTicks), fTicks), app.I(e1.ticks), lTicks)
  }

  def letNorm: NRule = {
    case Let(e1, e2, ticks) if ticks > 0 =>
      Let(e1, e2.I(ticks))
  }

  def nrule2wrule(nr: NRule): WRule = {case (_, t) if nr.isDefinedAt(t) => nr(t)}
  val normRules: List[NRule] = List(caseNorm, letRecNorm1, letNorm)
  val wRules = normRules.map(nrule2wrule)

  private def cycle(t: Term): Term = {
    var out = t
    for (wr <- wRules) {
      out = rewrite(wr, out)
    }
    out
  }

  def norm(t: Term): Term = {
    var before = t
    var after = t

    do {
      before = after
      after = cycle(before)
    } while (after != before)

    after
  }
}

// PART 2. Syntax operations
/**
 * Utility to work with nameless syntax (via indexes).
 * This implementation is based on the book "Types and Programming Languages".
 * Residuator utilizes the facility on nameless syntax to work with bound
 * variables directly.
 */
object NamelessSyntax {

  def rewrite(rule: PartialFunction[(Int, Term), Term], t: Term): Term = {
    // c - current context depth
    def walk(c: Int, t: Term): Term = t match {
      case x if rule isDefinedAt (c, t) => rule (c, t)
      case v: BVar             => v
      case v: FVar             => v
      case v: GVar             => v
      case Abs(t2, ticks)      => Abs(walk(c + 1, t2), ticks)
      case App(t1, t2, ticks)  => App(walk(c, t1), walk(c, t2), ticks)
      case Let(t1, t2, ticks)  => Let(walk(c, t1), walk(c + 1, t2), ticks)
      case Fix(t1, ticks)      => Fix(walk(c + 1, t1), ticks)
      case Case(t, bs, ticks)  => Case(walk(c, t), bs.map { case (ptr, ti) => (ptr, walk(c + ptr.args.size, ti)) }, ticks)
      case Ctr(n, args, ticks) => Ctr(n, args.map(walk(c, _)), ticks)
    }
    walk(0, t)
  }

  // Given a term t and a function onVar,
  // the result of tmMap onVar t is a term of the same shape as t
  // in which every *bound* variable has been replaced by the result of calling onVar on that variable.
  // c = initial "context depth"
  // onVar(c, v) - here c is current context depth
  private def tmMap(onVar: (Int, BVar) => Term, t: Term): Term = {
    // c - current context depth
    def walk(c: Int, t: Term): Term = t match {
      // SIC! onVar is responsible for adjusting ticks
      case v: BVar             => onVar(c, v)
      case v: FVar             => v
      case v: GVar             => v
      case Abs(t2, ticks)      => Abs(walk(c + 1, t2), ticks)
      case App(t1, t2, ticks)  => App(walk(c, t1), walk(c, t2), ticks)
      case Let(t1, t2, ticks)  => Let(walk(c, t1), walk(c + 1, t2), ticks)
      case Fix(t1, ticks)      => Fix(walk(c + 1, t1), ticks)
      case Case(t, bs, ticks)  => Case(walk(c, t), bs.map { case (ptr, ti) => (ptr, walk(c + ptr.args.size, ti)) }, ticks)
      case Ctr(n, args, ticks) => Ctr(n, args.map(walk(c, _)), ticks)
    }
    walk(0, t)
  }

  // in general case the domain of s may be bound var
  // (for example, in residuator)
  def applySubst(t: Term, s: Subst): Term = {
    def walk(c: Int, t: Term): Term = t match {
      case v: BVar                           => v
      // ticks propagation
      case v: FVar if s.get(v).isDefined     => Ticks.incrTicks(termShift(c, s(v)), v.ticks)
      case v: FVar                           => v
      case v: GVar                           => v
      case Abs(t2, ticks)                    => Abs(walk(c + 1, t2), ticks)
      case App(t1, t2, ticks)                => App(walk(c, t1), walk(c, t2), ticks)
      case Let(t1, t2, ticks)                => Let(walk(c, t1), walk(c + 1, t2), ticks)
      case Fix(t1, ticks)                    => Fix(walk(c + 1, t1), ticks)
      case Case(t, bs, ticks)                => Case(walk(c, t), bs.map { case (ptr, ti) => (ptr, walk(c + ptr.args.size, ti)) }, ticks)
      case Ctr(n, fs, ticks)                 => Ctr(n, fs.map(walk(c, _)), ticks)
    }
    walk(0, t)
  }

  // shifts unbound bvars by d
  // unbound bvars may appear during beta reduction (termSubstTop)
  def termShift(d: Int, t: Term): Term = {
    val f = { (c: Int, v: BVar) => if (v.i >= c) BVar(v.i + d, v.ticks) else v }
    tmMap(f, t)
  }

  // replaces BVar(0) in t by s
  private def termSubst(s: Term, t: Term): Term = {
    val onVar = { (c: Int, v: BVar) => if (v.i == c) Ticks.incrTicks(termShift(c, s), v.ticks) else v }
    tmMap(onVar, t)
  }

  // substitute s for 0-var in t
  // used for reductions
  def termSubstTop(s: Term, t: Term): Term =
    termShift(-1, termSubst(termShift(1, s), t))

  // can this subterm be extracted?
  def isFreeSubTerm(t: Term, depth: Int = 0): Boolean = t match {
    case BVar(i, _)       => i < depth
    case GVar(_, _)       => true
    case FVar(_, _)       => true
    case Abs(t1, _)       => isFreeSubTerm(t1, depth + 1)
    case App(t1, t2, _)   => isFreeSubTerm(t1, depth) && isFreeSubTerm(t2, depth)
    case Let(t1, t2, _)   => isFreeSubTerm(t1, depth) && isFreeSubTerm(t2, depth + 1)
    case Fix(t1, _)       => isFreeSubTerm(t1, depth + 1)
    case Case(sel, bs, _) => isFreeSubTerm(sel, depth) && bs.forall{ case (ptr, ti) => isFreeSubTerm(ti, depth + ptr.args.size)}
    case Ctr(n, fs, _)    => fs.forall(isFreeSubTerm(_, depth))
  }

  def findSubst(from: Term, to: Term): Option[Subst] =
    for (sub <- findSubst0(from, to))
    yield sub.filter { case (k, v) => k != v }

  def findSubst0(from: Term, to: Term): Option[Subst] = (from, to) match {
    case (fv: FVar, _) if isFreeSubTerm(to, 0) =>
      Some(Map(fv -> to))
    case (Abs(t1, _), Abs(t2, _)) =>
      findSubst0(t1, t2)
    case (App(h1, t1, _), App(h2, t2, _)) =>
      val s1 = findSubst0(h1, h2)
      val s2 = findSubst0(t1, t2)
      mergeOptSubst(s1, s2)
    case (Let(v1, t1, _), Let(v2, t2, _)) =>
      val s1 = findSubst0(v1, v2)
      val s2 = findSubst0(t1, t2)
      mergeOptSubst(s1, s2)
    case (Fix(t1, _), Fix(t2, _)) =>
      findSubst0(t1, t2)
    case (Case(sel1, bs1, _), Case(sel2, bs2, _)) =>
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
    case (Ctr(n1, fs1, _), Ctr(n2, fs2, _)) if n1 == n2 =>
      var sub: Option[Subst] = Some(Map())
      for ((t1, t2) <- fs1.zip(fs2)) {
        val sub1 = findSubst0(t1, t2)
        sub = mergeOptSubst(sub, sub1)
      }
      sub
    case (BVar(i, _), BVar(j, _)) if i == j =>
      Some(Map())
    case (GVar(i, _), GVar(j, _)) if i == j =>
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
    case fv @ FVar(_, _)  => List(fv)
    case BVar(_, _)       => List()
    case GVar(_, _)       => List()
    case Abs(t1, _)       => freeVars(t1)
    case App(t1, t2, _)   => List(freeVars(t1), freeVars(t2)).flatten.distinct
    case Let(t1, t2, _)   => List(freeVars(t1), freeVars(t2)).flatten.distinct
    case Fix(t1, _)       => freeVars(t1)
    case Ctr(_, args, _)  => args.map(freeVars).flatten.distinct
    case Case(sel, bs, _) => (freeVars(sel) :: bs.map(_._2).map(freeVars)).flatten.distinct
  }

  // here we mean subclass in semantical sense (as subset)
  val subclass: PartialOrdering[MetaTerm] = new SimplePartialOrdering[MetaTerm] {
    override def lteq(t1: MetaTerm, t2: MetaTerm) = (t1, t2) match {
      case (t1: Term, t2: Term) => findSubst(t2, t1).isDefined
      case _                    => false
    }
  }

}

// PART 3. Decomposition
sealed abstract class TermDecomposition
case class ObservableVar(v: FVar) extends TermDecomposition
case class ObservableVarApp(v: FVar, args: List[Term]) extends TermDecomposition
case class ObservableCon(c: Ctr) extends TermDecomposition
case class ObservableAbs(l: Abs) extends TermDecomposition

sealed abstract class Redex(term: Term)
case class RedexLamApp(lam: Abs, app: App) extends Redex(app)
case class RedexCall(f: GVar) extends Redex(f)
case class RedexLet(let: Let) extends Redex(let)
case class RedexFix(fix: Fix) extends Redex(fix)
case class RedexCaseCtr(c: Ctr, ce: Case) extends Redex(ce)
case class RedexCaseAlt(alt: Term, ce: Case) extends Redex(ce)

abstract case class Context(val redex: Redex) extends TermDecomposition {
  def replaceHole(t: Term): Term = t
}
private class ContextHole(override val redex: Redex) extends Context(redex)
private class ContextApp(head: Context, app: App) extends Context(head.redex) {
  override def replaceHole(t: Term) = App(head.replaceHole(t), app.t2)
}
private class ContextCase(selector: Context, ce: Case) extends Context(selector.redex) {
  override def replaceHole(t: Term) = Case(selector.replaceHole(t), ce.branches)
}

// Naive (slow) decomposition.
object Decomposition {
  def decompose(t: Term): TermDecomposition = try {
    linearApp(t) match {
      case Some((fv, args @ (_ :: _))) =>
        ObservableVarApp(fv, args)
      case _ => t match {
        case v: FVar => ObservableVar(v)
        case c: Ctr  => ObservableCon(c)
        case l: Abs  => ObservableAbs(l)
        case t       => createContext(t)
      }
    }
  } catch {
    case e: Exception => throw new Exception("cannot decompose " + t, e)
  }

  private def createContext(t: Term): Context = t match {
    case v: GVar                                 => new ContextHole(RedexCall(v))
    case let: Let                                => new ContextHole(RedexLet(let))
    case fix: Fix                                => new ContextHole(RedexFix(fix))
    case app @ App(l: Abs, arg, _)               => new ContextHole(RedexLamApp(l, app))
    case ce @ Case(v: FVar, _, _)                => new ContextHole(RedexCaseAlt(v, ce))
    case ce @ Case(a: App, _, _) if headVar_?(a) => new ContextHole(RedexCaseAlt(a, ce))
    case ce @ Case(c: Ctr, _, _)                 => new ContextHole(RedexCaseCtr(c, ce))
    case ce @ Case(s, _, _)                      => new ContextCase(createContext(s), ce)
    case a @ App(h, _, _)                        => new ContextApp(createContext(h), a)
    case _                                       => sys.error("unexpected context: " + t)
  }

  private def headVar_?(app: App): Boolean = app.t1 match {
    case v: FVar => true
    case a: App  => headVar_?(a)
    case _       => false
  }

  def linearApp(t: Term): Option[(FVar, List[Term])] = t match {
    case fv @ FVar(_, _) => Some((fv, List()))
    case App(h, a, _)    => linearApp(h) map { case (h, args) => (h, args :+ a) }
    case _               => None
  }
}
