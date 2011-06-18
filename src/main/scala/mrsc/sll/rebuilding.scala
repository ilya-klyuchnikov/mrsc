package mrsc.sll

import mrsc._

object SLLRebuilding {

  def gens(e: Expr): List[Let] =
    rebuildings(e) map { case (e1, sub) => Let(e1, sub toList) }

  def rebuildings(e: Expr): List[Rebuilding[Expr]] = e match {
    case Let(_, _) => Nil
    case _ => rebuild(e, Map.empty) filter nonTrivial
  }

  private def nonTrivial(rb: Rebuilding[Expr]): Boolean =
    rb match {
      case (Var(_), _) => false
      case (_, m) if m.isEmpty => false
      case (e, sub) =>
        val bodyVars = SLLExpressions.vars(e)
        sub forall {
          case (_, v @ Var(_)) => bodyVars contains v
          case _ => true
        }
    }

  private def rebuild(e: Expr, sub: Subst[Expr]): List[Rebuilding[Expr]] = {
    // already rebuilt
    val rbs1 = sub find { _._2 == e } map { case (v, _) => (Var(v), sub) } toList
    // new rebuilding 
    val rbs2 = {
      val fn = SLLExpressions.freshVar().name
      List((Var(fn), sub + (fn -> e)))
    }
    // rebuilding of arguments
    val rbs3 = e match {
      case FCall(n, xs) => rebuild1(xs, sub) map { case (ys, sub1) => (FCall(n, ys), sub1) }
      case GCall(n, xs) => rebuild1(xs, sub) map { case (ys, sub1) => (GCall(n, ys), sub1) }
      case Ctr(n, xs) => rebuild1(xs, sub) map { case (ys, sub1) => (Ctr(n, ys), sub1) }
      case _ => List((e, sub))
    }
    rbs1 ++ rbs2 ++ rbs3
  }

  private def rebuild1(es: List[Expr], sub: Subst[Expr]): List[(List[Expr], Subst[Expr])] = {
    es.foldRight((List[Expr](), sub) :: Nil) { (e, acc) =>
      for { (es1, sub) <- acc; (t, sub1) <- rebuild(e, sub) } yield (t :: es1, sub1)
    }
  }

}