package mrsc.pfp

import mrsc.core._

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

}

trait VarGen {
  var freeVar: Int = 10
  def nextVar(x: Any = ()): FVar = {
    freeVar += 1
    FVar(freeVar)
  }
}

trait Rebuildings extends VarGen {

  protected def termRebuildings(t: Term): List[Rebuilding] = rebuild(t, Map.empty)

  private def rebuild(e: Term, sub: Subst): List[Rebuilding] = {

    val rbs1: List[Rebuilding] = e match {
      case Abs(body) =>
        for { Rebuilding(t1, sub1) <- rebuild(body, sub) }
          yield Rebuilding(Abs(t1), sub1)
      case App(a1, a2) =>
        for {
          Rebuilding(t1, sub1) <- rebuild(a1, sub)
          Rebuilding(t2, sub2) <- rebuild(a2, sub1)
        } yield Rebuilding(App(t1, t2), sub2)
      case Let(a1, a2) =>
        for {
          Rebuilding(t1, sub1) <- rebuild(a1, sub)
          Rebuilding(t2, sub2) <- rebuild(a2, sub1)
        } yield Rebuilding(Let(t1, t2), sub2)
      case Fix(body) =>
        for { Rebuilding(t1, sub1) <- rebuild(body, sub) }
          yield Rebuilding(Fix(t1), sub1)
      case Ctr(n, xs) =>
        for { (ys, sub1) <- rebuild1(xs, sub) }
          yield Rebuilding(Ctr(n, ys), sub1)
      case Case(sel, bs) =>
        val (pts, bodies) = bs.unzip
        for {
          Rebuilding(sel1, sub1) <- rebuild(sel, sub)
          (bodies2, sub2) <- rebuild1(bodies, sub1)
        } yield Rebuilding(Case(sel1, pts zip bodies2), sub2)
      case _ =>
        List(Rebuilding(e, sub))
    }

    // extracting a term itself if it is extractable 
    val rbs2 =
      if (NamelessSyntax.isFreeSubTerm(e)) {
        val fn = nextVar()
        List(Rebuilding(fn, sub + (fn -> e)))
      } else
        List()

    // term is already extracted
    val rbs3 = for { (k, e1) <- sub if e1 == e } yield Rebuilding(k, sub)

    rbs1 ++ rbs2 ++ rbs3
  }

  // all combinations of rebuildings a list of expressions 
  private def rebuild1(es: List[Term], sub: Subst): List[(List[Term], Subst)] =
    (es :\ ((List[Term](), sub) :: Nil)) { (e, acc) =>
      for { (es1, sub) <- acc; Rebuilding(t, sub1) <- rebuild(e, sub) } yield (t :: es1, sub1)
    }
}

// delegating everything to syntax
trait PFPSyntax extends Rebuildings {
  def subst(c: Term, sub: Subst): Term =
    NamelessSyntax.applySubst(c, sub)
  def findSubst(from: Term, to: Term): Option[Subst] =
    NamelessSyntax.findSubst(from, to)
  def rebuildings(t: MetaTerm): List[Rebuilding] = t match {
    case t: Term => distinct(termRebuildings(t) filterNot trivialRb(t))
    case _       => List()
  }
  
  private def distinct(rbs: List[Rebuilding]): List[Rebuilding] = {
    var result: List[Rebuilding] = Nil
    val seen = scala.collection.mutable.HashSet[Rebuilding]()
    for (x <- rbs) {
      if (seen.find(r => subclass.equiv(r.t, x.t)).isEmpty) {
        result = x :: result
        seen += x
      }
    }
    result
  }
  
  // forbid to extract a single variable
  private def trivialRb(c: MetaTerm)(rb: Rebuilding) =
    (rb.sub.values.toSet + rb.t) exists { subclass.equiv(c, _) }

  // here we mean subclass in semantical sense (as subset)
  val subclass: PartialOrdering[MetaTerm] = new SimplePartialOrdering[MetaTerm] {
    override def lteq(t1: MetaTerm, t2: MetaTerm) = (t1, t2) match {
      case (t1: Term, t2: Term) => findSubst(t2, t1).isDefined
      case _                    => false
    }
  }
}

// Driving without positive information propagation
trait PFPSemantics extends VarGen {
  import NamelessSyntax._
  val gc: GContext
  def driveStep(t: MetaTerm): MStep = t match {
    case rb: Rebuilding =>
      DecomposeRebuildingMStep(rb)
    case t: Term => Decomposition.decompose(t) match {
      case ObservableVar(v) =>
        StopMStep
      case ObservableCon(c) =>
        DecomposeCtrMStep(c)
      case ObservableAbs(l) =>
        val fv = nextVar()
        val body1 = termSubstTop(fv, l.t)
        DecomposeAbsMStep(body1, fv)
      case ObservableVarApp(fv, args) =>
        DecomposeVarApp(fv, args)
      case context @ Context(RedexCall(f)) =>
        UnfoldMStep(context.replaceHole(gc(f.n)))
      case context @ Context(RedexFix(t1 @ Fix(Abs(body)))) =>
        UnfoldMStep(context.replaceHole(termSubstTop(t1, body)))
      case context @ Context(RedexLamApp(Abs(t1), App(_, t2))) =>
        TransientMStep(context.replaceHole((termSubstTop(t2, t1))))
      case context @ Context(RedexCaseCtr(Ctr(name, args), Case(_, bs))) =>
        val Some((ptr, body)) = bs.find(_._1.name == name)
        val next = args.foldRight(body)(termSubstTop(_, _))
        TransientMStep(context.replaceHole(next))
      case context @ Context(RedexCaseAlt(v: FVar, Case(_, bs))) =>
        val xs = for { (ptr @ Ptr(name, args), body) <- bs } yield {
          val ctr = Ctr(name, args.map(nextVar))
          val next = ctr.args.foldRight(body)(termSubstTop(_, _))
          (ptr, ctr, context.replaceHole(next))
        }
        VariantsMStep(v, xs)
      case context @ Context(RedexCaseAlt(sel, Case(_, bs))) =>
        val v = nextVar()
        RebuildMStep(Rebuilding(context.replaceHole(Case(v, bs)), Map(v -> sel)))
      case context @ Context(RedexFix(t1 @ Fix(_))) =>
        sys.error("unexpected term")
      case context @ Context(RedexLet(Let(v, body))) =>
        val red1 = termSubstTop(v, body)
        TransientMStep(context.replaceHole(red1))
    }
  }
}

trait MutualGens extends PFPSyntax {
  def mutualGens(c1: MetaTerm, c2: MetaTerm): List[Rebuilding] = {
    val nonTrivialRbs = rebuildings(c1)
    nonTrivialRbs filter { rb => subclass.gteq(rb.t, c2) }
  }
}

trait MSG extends MutualGens {
  def msg(c1: MetaTerm, c2: MetaTerm): Option[Rebuilding] = {
    val mutual = mutualGens(c1, c2)
    mutual find { rb => mutual forall { other => subclass.lteq(rb.t, other.t) } }
  }
}

trait SimplePartialOrdering[T] extends PartialOrdering[T] {
  override def tryCompare(x: T, y: T): Option[Int] = (lteq(x, y), lteq(y, x)) match {
    case (false, false) =>
      None
    case (false, true) =>
      Some(1)
    case (true, false) =>
      Some(-1)
    case (true, true) =>
      Some(0)
  }
}
