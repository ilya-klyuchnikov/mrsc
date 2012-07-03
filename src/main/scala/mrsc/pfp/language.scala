package mrsc.pfp

import mrsc.core._

// delegating everything to syntax
trait PFPSyntax extends Rebuildings {
  def subst(c: Term, sub: Subst): Term =
    Syntax.applySubst(c, sub)
  def findSubst(from: Term, to: Term): Option[Subst] =
    Syntax.findSubst(from, to)
  def rebuildings(t: MetaTerm): List[Rebuilding] = t match {
    case t: Term => termRebuildings(t)
    case _       => List()
  }

  def trivialRb(c: MetaTerm)(rb: Rebuilding) =
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
  import Syntax._
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
        TransientMStep(context.replaceHole(gc(f.n)))
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
        RebuildMStep(Rebuilding(context.replaceHole(v), Map(v -> sel)))
      case context @ Context(RedexFix(t1 @ Fix(Abs(body)))) =>
        TransientMStep(context.replaceHole(termSubstTop(t1, body)))
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
    val nonTrivialRbs = rebuildings(c1) filterNot trivialRb(c1)
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
