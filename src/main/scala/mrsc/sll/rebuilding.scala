package mrsc.sll

import mrsc._

object SLLRebuilding {

  def gens(e: Expr): List[Let] =
    rebuildings(e) map { case (e1, sub) => Let(e1, sub toList) }

  def rebuildings(e: Expr): List[Rebuilding[Expr]] = e match {
    case Let(_, _) => 
      Nil
    case _ =>
      // There may be duplicates up to renaming when there are repeated variables.
      // One may filter such duplicates further.
      rebuild(e, Map.empty) filter nonTrivial
  }

  private def nonTrivial(rb: Rebuilding[Expr]): Boolean =
    rb match {
      case (Var(_), _) => false
      case (_, m) if m.isEmpty => false
      case (e, sub) =>
        val bodyVars = SLLSyntax.vars(e)
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
      val fn = newName(e, rbs1.size)
      List((Var(fn), sub + (fn -> e)))
    }
    // rebuilding of arguments
    val rbs3 = e match {
      case FCall(n, xs) => rebuild1(xs, sub) map { case (ys, sub1) => (FCall(n, ys), sub1) }
      case GCall(n, xs) => rebuild1(xs, sub) map { case (ys, sub1) => (GCall(n, ys), sub1) }
      case Ctr(n, xs) => rebuild1(xs, sub) map { case (ys, sub1) => (Ctr(n, ys), sub1) }
      case _ => List((e, sub))
    }
    rbs3 ++ rbs1 ++ rbs2
  }

  private def rebuild1(es: List[Expr], sub: Subst[Expr]): List[(List[Expr], Subst[Expr])] = {
    es.foldRight((List[Expr](), sub) :: Nil) { (e, acc) =>
      for { (es1, sub) <- acc; (t, sub1) <- rebuild(e, sub) } yield (t :: es1, sub1)
    }
  }
  
  private def newName(e: Expr, seed: Int): Name =
    "gen/" + (seed + 1) + "/" + e.toString()
}