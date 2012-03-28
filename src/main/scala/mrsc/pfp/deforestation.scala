package mrsc.pfp

import mrsc.core._
import Syntax._

abstract sealed trait DeforestStep
case object TransientStep extends DeforestStep
case object CaseStep extends DeforestStep
case object CtrStep extends DeforestStep

case class Deforester(gc: GContext) extends GraphRewriteRules[Term, DeforestStep] {
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
    case Ctr(_, fs) =>
      fs.map(f => (f._2, CtrStep))
    case App(Abs(t1), t2) =>
      List((termSubstTop(t2, t1), TransientStep))
    case App(t1, t2) =>
      for ((n, s) <- driveStep(t1))
        yield (App(n, t2), s)
    case Fix(Abs(body)) =>
      List((termSubstTop(t, body), TransientStep))
    case Fix(Ctr(_, _)) =>
      error("unexpected term: " + t)
    case Fix(t1) =>
      for ((n, s) <- driveStep(t1))
        yield (Fix(n), s)
    case DeCtr(Ctr(n, fs), f) =>
      List((fs.find(_._1 == f).get._2, TransientStep))
    case DeCtr(t1, f) =>
      for ((n, s) <- driveStep(t1))
        yield (DeCtr(n, f), s)
    case Case(ctr @ Ctr(tag, fs), bs) =>
      // Not good code for now
      val Some((_, body)) = bs.find(_._1 == tag)
      var t1 = termSubstTop(FVar("$"), body)
      for ((fi, ti) <- fs) {
        t1 = replace(t1, DeCtr(FVar("$"), fi), ti)
      }
      //t1 = replace(t1, FVar("$"), ctr)
      List((t1, TransientStep))
    case Case(t1, bs) if isCaseable(t1) =>
      for ((li, ti) <- bs)
        yield (termSubstTop(t1, ti), CaseStep)
    case Case(t1, bs) =>
      for ((n, s) <- driveStep(t1))
        yield (Case(n, bs), s)
    case _ =>
      error("unexpected term: " + t)
  }

  private def isCaseable(t: Term): Boolean = t match {
    case FVar(_)      => true
    case DeCtr(t1, _) => isCaseable(t1)
    case _            => false
  }

  def fold(g: G): Option[S] = {
    val conf = g.current.conf
    val base = g.current.ancestors.find(n => Syntax.renaming(n.conf, conf))
    base.map(n => FoldStep(n.sPath))
  }
}