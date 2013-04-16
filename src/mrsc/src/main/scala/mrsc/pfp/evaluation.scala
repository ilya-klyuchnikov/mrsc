package mrsc.pfp

import NamelessSyntax._

/**
 * Call-by-name step-by-step evaluation.
 * The intended use of this module is for extensive testing
 */
object CBNEval {

  def isLazyVal(t: Term) = t match {
    case Abs(_)    => true
    case Ctr(_, _) => true
    case _         => false
  }

  def isVal(t: Term): Boolean = t match {
    case Abs(_)       => true
    case Ctr(_, args) => args.forall(isVal)
    case _            => false
  }

  // one-step reduction evaluation
  def lazyStep(t: Term, g: GContext): Term = t match {
    case _ if isLazyVal(t) =>
      t
    case GVar(n) =>
      g(n)
    case Case(Ctr(name, args), bs) =>
      val Some((ptr, body)) = bs.find(_._1.name == name)
      args.foldRight(body)(termSubstTop(_, _))
    case Case(t1, bs) =>
      Case(lazyStep(t1, g), bs)
    case App(Abs(t1), t2) =>
      termSubstTop(t2, t1)
    case App(t1, t2) =>
      App(lazyStep(t1, g), t2)
    case Let(v, body) =>
      termSubstTop(v, body)
    case Fix(body) =>
      termSubstTop(t, body)
    case _ =>
      sys.error("unexpected term: " + t)
  }

  def eval(t: Term, g: GContext): Term = lazyStep(t, g) match {
    case Abs(_) =>
      t
    case Ctr(n, fs) =>
      Ctr(n, fs.map(eval(_, g)))
    case t1 =>
      eval(t1, g)
  }
}