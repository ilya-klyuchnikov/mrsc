package mrsc.pfp.sll

object Decomposition {
  abstract sealed class Dec
  case class DecLet(let: Let) extends Dec
  sealed trait Observable extends Dec
  case class ObservableCtr(c: Ctr) extends Observable
  case class ObservableVar(v: Var) extends Observable

  // maybe not so good - abstract case class
  abstract case class Context(red: Redex) extends Dec {
    def replaceRedex(t: Expr): Expr
  }
  
  private class ContextHole(override val red: Redex) extends Context(red) {
    def replaceRedex(t: Expr): Expr = t
  }
  private class ContextGCall(gcall: GCall, context: Context) extends Context(context.red) {
    def replaceRedex(t: Expr): Expr = GCall(gcall.name, context.replaceRedex(t) :: gcall.args.tail)
  }

  sealed abstract class Redex(term: Expr)
  case class RedexFCall(fcall: FCall) extends Redex(fcall)
  case class RedexGCallCtr(gcall: GCall, ctr: Ctr) extends Redex(gcall)
  case class RedexGCallVar(gcall: GCall, vrb: Var) extends Redex(gcall)

  def decompose(t: Expr): Dec = t match {
    case l: Let => DecLet(l)
    case v: Var => ObservableVar(v)
    case c: Ctr => ObservableCtr(c)
    case f: FCall => new ContextHole(RedexFCall(f))
    case g: GCall => processGCall(g)
  }

  private def processGCall(g: GCall): Context = g.args.head match {
    case g1: GCall => new ContextGCall(g, processGCall(g1))
    case f: FCall => new ContextGCall(g, new ContextHole(RedexFCall(f)))
    case v: Var => new ContextHole(RedexGCallVar(g, v))
    case c: Ctr => new ContextHole(RedexGCallCtr(g, c))
  }
}