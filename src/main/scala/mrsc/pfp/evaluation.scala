package mrsc.pfp

import Syntax._

// call-by-name step-by-step
// evaluation
object CBNEval {

  def isLazyVal(t: Term) = t match {
    case Abs(_)    => true
    case Ctr(_, _) => true
    case _         => false
  }

  def isVal(t: Term): Boolean = t match {
    case Abs(_)     => true
    case Ctr(_, fs) => fs.forall(f => isVal(f._2))
    case _          => false
  }

  // one-step reduction
  // evaluation
  def lazyStep(t: Term, g: GContext): Term = t match {
    case _ if isLazyVal(t) =>
      t
    case GVar(n) =>
      g(n)
    case Case(ctr @ Ctr(tag, _), bs) =>
      val Some((_, body)) = bs.find(_._1 == tag)
      termSubstTop(ctr, body)
    case Case(t1, bs) =>
      Case(lazyStep(t1, g), bs)
    case App(Abs(t1), t2) =>
      termSubstTop(t2, t1)
    case App(t1, t2) =>
      App(lazyStep(t1, g), t2)
    case Let(v, body) =>
      termSubstTop(v, body)
    case Fix(Abs(body)) =>
      termSubstTop(t, body)
    case Fix(Ctr(_, _)) =>
      error("unexpected term: " + t)
    case Fix(t1) =>
      Fix(lazyStep(t1, g))
    case DeCtr(Ctr(n, fs), f) =>
      fs.find(_._1 == f).get._2
    case DeCtr(t1, f) =>
      DeCtr(lazyStep(t1, g), f)
    case _ =>
      error("unexpected term: " + t)
  }

  def eval(t: Term, g: GContext): Term = lazyStep(t, g) match {
    case Abs(_) =>
      t
    case Ctr(n, fs) =>
      Ctr(n, fs.map { case (f, t1) => (f, eval(t1, g)) })
    case t1 =>
      eval(t1, g)
  }
}