package mrsc.pfp

sealed abstract class TermDecomposition
sealed abstract class Observable extends TermDecomposition
case class ObservableVar(v: FVar) extends Observable
case class ObservableVarApp(v: FVar, args: List[Term]) extends Observable
case class ObservableCon(c: Ctr) extends Observable
case class ObservableLam(l: Abs) extends Observable

sealed abstract class Redex(term: Term)
case class RedexLamApp(lam: Abs, app: App) extends Redex(app)
case class RedexCaseCon(c: Ctr, ce: Case) extends Redex(ce)
case class RedexCall(f: GVar) extends Redex(f)
case class RedexLet(let: Let) extends Redex(let)
case class RedexFix(fix: Fix) extends Redex(fix)
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
  def decompose(t: Term): TermDecomposition = try {
    linearApp(t) match {
      case Some((fv, args @ (_ :: _))) =>
        ObservableVarApp(fv, args)
      case _ => t match {
        case v: FVar => ObservableVar(v)
        case c: Ctr  => ObservableCon(c)
        case l: Abs  => ObservableLam(l)
        case t       => createContext(t)
      }
    }

  } catch {
    case e => throw new Exception("cannot decompose " + t, e)
  }

  private def createContext(t: Term): Context =
    t match {
      case v: GVar                                      => new ContextHole(RedexCall(v))
      case let: Let                                     => new ContextHole(RedexLet(let))
      case fix: Fix                                     => new ContextHole(RedexFix(fix))
      case app @ App(l: Abs, arg)                       => new ContextHole(RedexLamApp(l, app))
      case ce @ Case(v: FVar, _)                        => new ContextHole(RedexCaseVar(v, ce))
      case ce @ Case(a: App, _) if (headVar(a) != null) => new ContextHole(RedexCaseVar(a, ce))
      case ce @ Case(c: Ctr, _)                         => new ContextHole(RedexCaseCon(c, ce))
      case ce @ Case(s, _)                              => new ContextCase(createContext(s), ce)
      case a @ App(h, _)                                => new ContextApp(createContext(h), a)
      case v => throw new IllegalArgumentException("cannot be decomposed as a context: " + v)
    }

  private def headVar(app: App): FVar = app.t1 match {
    case v: FVar => v
    case a: App  => headVar(a)
    case _       => null
  }

  def linearApp(t: Term): Option[(FVar, List[Term])] = t match {
    case fv @ FVar(_) => Some((fv, List()))
    case App(h, a)    => linearApp(h) map { case (h, args) => (h, args :+ a) }
    case _            => None
  }
}