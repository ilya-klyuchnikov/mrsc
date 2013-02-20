package mrsc.pfp

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
    case v: GVar                              => new ContextHole(RedexCall(v))
    case let: Let                             => new ContextHole(RedexLet(let))
    case fix: Fix                             => new ContextHole(RedexFix(fix))
    case app @ App(l: Abs, arg)               => new ContextHole(RedexLamApp(l, app))
    case ce @ Case(v: FVar, _)                => new ContextHole(RedexCaseAlt(v, ce))
    case ce @ Case(a: App, _) if headVar_?(a) => new ContextHole(RedexCaseAlt(a, ce))
    case ce @ Case(c: Ctr, _)                 => new ContextHole(RedexCaseCtr(c, ce))
    case ce @ Case(s, _)                      => new ContextCase(createContext(s), ce)
    case a @ App(h, _)                        => new ContextApp(createContext(h), a)
    case _                                    => sys.error("unexpected context: " + t)
  }

  private def headVar_?(app: App): Boolean = app.t1 match {
    case v: FVar => true
    case a: App  => headVar_?(a)
    case _       => false
  }

  def linearApp(t: Term): Option[(FVar, List[Term])] = t match {
    case fv @ FVar(_) => Some((fv, List()))
    case App(h, a)    => linearApp(h) map { case (h, args) => (h, args :+ a) }
    case _            => None
  }
}