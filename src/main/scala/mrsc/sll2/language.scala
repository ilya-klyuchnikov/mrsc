package mrsc.sll

import mrsc._

object SLLSyntax extends Syntax[Expr] {
  override val instance: PartialOrdering[Expr] = new SimplePartialOrdering[Expr] {
    override def lteq(x: Expr, y: Expr) = SLLExpressions.inst(x, y)
  }

  override def subst(c: Expr, sub: Subst[Expr]): Expr = {
    val sllSub = sub map { kv => ((Var(kv._1), kv._2)) }
    SLLExpressions.subst(c, sllSub)
  }

  override def rebuildings(e: Expr): List[Rebuilding[Expr]] =
    SLLGeneralizations.gens2(e)

  override def rebuilding2Configuration(rebuilding: Rebuilding[Expr]): Expr = {
    val (e, sub) = rebuilding
    val bindings = sub.toList map { case (n, e) => (Var(n), e) }
    Let(e, bindings)
  }

  override def findSubst(from: Expr, to: Expr): Option[Subst[Expr]] = {
    val sllSub = SLLExpressions.findSubst(from, to)
    if (sllSub == null) {
      None
    } else {
      val sub = sllSub map { case (k, v) => (k.name, v) }
      Some(sub)
    }
  }

}