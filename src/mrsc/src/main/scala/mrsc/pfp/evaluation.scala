package mrsc.pfp

import NamelessSyntax._

/**
 * Call-by-name step-by-step evaluation.
 * The intended use of this module is for extensive testing
 */
object CBNEval {

  def isLazyVal(t: Term) = t match {
    case Abs(_, _)    => true
    case Ctr(_, _, _) => true
    case _         => false
  }

  def isVal(t: Term): Boolean = t match {
    case Abs(_, _)       => true
    case Ctr(_, args, _) => args.forall(isVal)
    case _            => false
  }

  // one-step reduction evaluation
  def lazyStep(t: Term, g: GContext): Term = t match {
    case _ if isLazyVal(t) =>
      t
    case GVar(n, _) =>
      g(n)
    case Case(Ctr(name, args, _), bs, _) =>
      val Some((ptr, body)) = bs.find(_._1.name == name)
      args.foldRight(body)(termSubstTop(_, _))
    case Case(t1, bs, _) =>
      Case(lazyStep(t1, g), bs)
    case App(Abs(t1, _), t2, _) =>
      termSubstTop(t2, t1)
    case App(t1, t2, _) =>
      App(lazyStep(t1, g), t2)
    case Let(v, body, _) =>
      termSubstTop(v, body)
    case Fix(body, _) =>
      termSubstTop(t, body)
    case _ =>
      sys.error("unexpected term: " + t)
  }

  def eval(t: Term, g: GContext): Term = lazyStep(t, g) match {
    case Abs(_, _) =>
      t
    case Ctr(n, fs, _) =>
      Ctr(n, fs.map(eval(_, g)))
    case t1 =>
      eval(t1, g)
  }
}

// Calculate ticks of execution for original program.
// Implies that original program is without ticks.
object CBNEvalWithTicks {

  def isLazyVal(t: Term) = t match {
    case Abs(_, _)    => true
    case Ctr(_, _, _) => true
    case _         => false
  }

  def isVal(t: Term): Boolean = t match {
    case Abs(_, _)       => true
    case Ctr(_, args, _) => args.forall(isVal)
    case _            => false
  }

  // one-step reduction evaluation
  def lazyStep(t: Term, g: GContext): (Int, Term) = t match {
    case _ if isLazyVal(t) =>
      (0, t)
    case GVar(n, _) =>
      (1, g(n))
    case Case(Ctr(name, args, _), bs, _) =>
      val Some((ptr, body)) = bs.find(_._1.name == name)
      (0, args.foldRight(body)(termSubstTop(_, _)))
    case Case(t1, bs, _) =>
      val (ticks, evaled) = lazyStep(t1, g)
      (ticks, Case(evaled, bs))
    case App(Abs(t1, _), t2, _) =>
      (0, termSubstTop(t2, t1))
    case App(t1, t2, _) =>
      val (ticks, evaled) = lazyStep(t1, g)
      (ticks, App(evaled, t2))
    case Let(v, body, _) =>
      (0, termSubstTop(v, body))
    case Fix(body, _) =>
      (1, termSubstTop(t, body))
    case _ =>
      sys.error("unexpected term: " + t)
  }

  def eval(t: Term, g: GContext): (Int, Term) = lazyStep(t, g) match {
    case (ticks, Abs(_, _)) =>
      (ticks, t)
    case (ticks, Ctr(n, fs, _)) =>
      val (ts, args) = fs.map(eval(_, g)).unzip
      (ticks + ts.sum, Ctr(n, args))
    case (ticks1, t1) =>
      val (ticks2, t2) = eval(t1, g)
      (ticks1 + ticks2, t2)
  }
}

// Tries to compute ticks of execution of original program based on the
// residual program.
// TODO: it is likely that current code is incorrect and too naive.
object CBNEvalWithTicksResidual {

  def isLazyVal(t: Term) = t match {
    case Abs(_, _)    => true
    case Ctr(_, _, _) => true
    case _         => false
  }

  def isVal(t: Term): Boolean = t match {
    case Abs(_, _)       => true
    case Ctr(_, args, _) => args.forall(isVal)
    case _            => false
  }

  // one-step reduction evaluation
  def lazyStep(t: Term, g: GContext): (Int, Term) = t match {
    case _ if isLazyVal(t) =>
      (t.ticks, t)
    case Case(Ctr(name, args, _), bs, _) =>
      val Some((ptr, body)) = bs.find(_._1.name == name)
      (t.ticks, args.foldRight(body)(termSubstTop(_, _)))
    case Case(t1, bs, ticks) =>
      val (ticks1, evaled) = lazyStep(t1, g)
      (ticks + ticks1, Case(evaled, bs))
    case App(Abs(t1, ticks1), t2, ticks) =>
      (ticks + ticks1, termSubstTop(t2, t1))
    case App(t1, t2, ticks) =>
      val (ticks1, evaled) = lazyStep(t1, g)
      (ticks + ticks1, App(evaled, t2))
    case Let(v, body, ticks) =>
      (ticks, termSubstTop(v, body))
    case Fix(body, ticks) =>
      (ticks, termSubstTop(t, body))
    case _ =>
      sys.error("unexpected term: " + t)
  }

  def eval(t: Term, g: GContext): (Int, Term) = lazyStep(t, g) match {
    case (ticks, Abs(_, _)) =>
      (ticks, t)
    case (ticks, Ctr(n, fs, _)) =>
      val (ts, args) = fs.map(eval(_, g)).unzip
      (ticks + ts.sum, Ctr(n, args))
    case (ticks1, t1) =>
      val (ticks2, t2) = eval(t1, g)
      (ticks1 + ticks2, t2)
  }
}
