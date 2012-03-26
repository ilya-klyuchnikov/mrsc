package mrsc.pfp

import Syntax._

object CBNEval {

  def isVal(t: Term) = t match {
    case Abs(_, _) => true
    case Ctr(_, _) => true
    case _         => false
  }

  // one-step reduction
  // evaluation
  def step(t: Term, g: GContext): Term = t match {
    case GVar(n) =>
      g(n)
    case Case(ctr @ Ctr(tag, _), bs) =>
      val Some((_, body)) = bs.find(_._1 == tag)
      termSubstTop(ctr, body)
    case Case(t1, bs) =>
      Case(step(t1, g), bs)
    case App(Abs(x, t1), t2) =>
      termSubstTop(t2, t1)
    case App(t1, t2) =>
      App(step(t1, g), t2)
    case Let(x, v, body) =>
      termSubstTop(v, body)
    case Fix(Abs(_, body)) =>
      termSubstTop(t, body)
    case Fix(Ctr(_, _)) =>
      error("unexpected term: " + t)
    case Fix(t1) =>
      Fix(step(t1, g))
    case _ =>
      error("unexpected term: " + t)
  }
}