package mrsc.pfp

sealed abstract class TermDecomposition
sealed abstract class Observable(val term: Term) extends TermDecomposition
case class ObservableVar(v: FVar) extends Observable(v)
case class ObservableVarApp(v: FVar, app: App) extends Observable(app)
case class ObservableCon(c: Ctr) extends Observable(c)
case class ObservableLam(l: Abs) extends Observable(l)

sealed abstract class Redex(term: Term)
case class RedexLamApp(lam: Abs, app: App) extends Redex(app)
case class RedexCaseCon(c: Ctr, ce: Case) extends Redex(ce)
case class RedexCall(f: GVar) extends Redex(f)
case class RedexCaseVar(v: Term, ce: Case) extends Redex(ce)

abstract case class Context(val redex: Redex) extends TermDecomposition {
  def replaceHole(t: Term): Term
}
private class ContextHole(override val redex: Redex) extends Context(redex) {
  def replaceHole(t: Term) = t
}
private class ContextApp(head: Context, app: App) extends Context(head.redex) {
  def replaceHole(t: Term) = App(head.replaceHole(t), app.t2)
}
private class ContextCase(selector: Context, ce: Case) extends Context(selector.redex) {
  def replaceHole(t: Term) = Case(selector.replaceHole(t), ce.branches)
}

object Decomposition {
  def decompose(t: Term): TermDecomposition = t match {
    case c: Ctr                       => ObservableCon(c)
    case l: Abs                       => ObservableLam(l)
    case v: FVar                      => ObservableVar(v)
    case a: App if headVar(a) != null => ObservableVarApp(headVar(a), a)
    case t                            => createContext(t)
  }

  private def createContext(t: Term): Context = t match {
    case v: GVar => new ContextHole(RedexCall(v))
    case app @ App(l: Abs, arg) => new ContextHole(RedexLamApp(l, app))
    case ce @ Case(v: FVar, _) => new ContextHole(RedexCaseVar(v, ce))
    case ce @ Case(a: App, _) if (headVar(a) != null) => new ContextHole(RedexCaseVar(a, ce))
    case ce @ Case(c: Ctr, _) => new ContextHole(RedexCaseCon(c, ce))
    case ce @ Case(s, _) => new ContextCase(createContext(s), ce)
    case a @ App(h, _) => new ContextApp(createContext(h), a)
    case v => throw new IllegalArgumentException("cannot be decomposed as a context: " + v)
  }

  private def headVar(app: App): FVar = app.t1 match {
    case v: FVar => v
    case a: App  => headVar(app)
    case _       => null
  }
}