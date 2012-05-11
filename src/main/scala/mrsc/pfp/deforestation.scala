package mrsc.pfp

import mrsc.core._
import Syntax._

abstract sealed trait DeforestStep
case object TransientStep extends DeforestStep {
  override def toString = "->"
}
case class CaseBranch(term: Term, ptr: Ptr, alt: Ctr) extends DeforestStep {
  override def toString = term + " = " + alt
}
case object CtrArg extends DeforestStep {
  override def toString = ""
}

case class Deforester(gc: GContext) extends GraphRewriteRules[Term, DeforestStep] {
  // TODO: dirty hack for now (not functional approach)
  var freeVar: Int = 30
  private def nextVar(x: Any = null): FVar = {
    freeVar += 1
    FVar(freeVar)
  }

  override def steps(g: G): List[S] = fold(g) match {
    case Some(s) => List(s)
    case None    => List(drive(g))
  }

  def drive(g: G): S =
    AddChildNodesStep(driveStep(g.current.conf))

  def driveStep(t: Term): List[(Term, DeforestStep)] = t match {
    case GVar(n) =>
      List((gc(n), TransientStep))
    case FVar(n) =>
      List()
    case Ctr(n, args) =>
      args.map(a => (a, CtrArg))
    case App(Abs(t1), t2) =>
      List((termSubstTop(t2, t1), TransientStep))
    case App(t1, t2) =>
      for ((n, s) <- driveStep(t1))
        yield (App(n, t2), s)
    case Fix(Abs(body)) =>
      List((termSubstTop(t, body), TransientStep))
    case Fix(Ctr(_, _)) =>
      sys.error("unexpected term: " + t)
    case Fix(t1) =>
      for ((n, s) <- driveStep(t1))
        yield (Fix(n), s)
    case Case(Ctr(name, args), bs) =>
      val Some((ptr, body)) = bs.find(_._1.name == name)
      val next = args.foldRight(body)(termSubstTop(_, _))
      List((next, TransientStep))
    case Case(t @ FVar(_), bs) =>
      val bSteps = for { (ptr@Ptr(name, args), body) <- bs } yield {
        val ctr = Ctr(name, args.map(nextVar))
        val next = ctr.args.foldRight(body)(termSubstTop(_, _))
        (next, CaseBranch(t, ptr, ctr))
      }
      bSteps
    case Case(t1, bs) =>
      for ((n, s) <- driveStep(t1))
        yield (Case(n, bs), s)
    case Let(v @ Fix(_), body) =>
      List((termSubstTop(v, body), TransientStep))
    case _ =>
      sys.error("unexpected term: " + t)
  }

  def fold(g: G): Option[S] = {
    val conf = g.current.conf
    val base = g.current.ancestors.find(n => Syntax.renaming(n.conf, conf))
    base.map(n => FoldStep(n.sPath))
  }
}