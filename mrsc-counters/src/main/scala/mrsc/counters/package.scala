package mrsc

package object counters {
  type Conf = List[Expr]
  type TransitionRule = PartialFunction[Conf, Conf]
  implicit def intToExpr(i: Int): Expr = Num(i)
  val ϖ = Omega
}
