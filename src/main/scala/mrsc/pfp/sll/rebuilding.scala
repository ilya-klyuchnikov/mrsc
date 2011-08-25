package mrsc.pfp.sll

import mrsc.pfp._

object SLLRebuilding {

  def gens(e: Expr): List[Let] =
    rebuildings(e) map { case (e1, sub) => Let(e1, sub toList) }

  def rebuildings(e: Expr): List[RawRebuilding[Expr]] = e match {
    case Let(_, _) =>
      Nil
    case _ =>
      // There may be duplicates up to renaming when there are repeated variables.
      // One may filter such duplicates further.
      distinct(rebuild(e, e, Map.empty).filter(nonTrivial))
  }
  
  def distinct(es: List[RawRebuilding[Expr]]): List[RawRebuilding[Expr]] = {
    import scala.collection.mutable.ListBuffer
    val ls = ListBuffer[RawRebuilding[Expr]]()
    for (e <- es) {
      if (ls.find(e1 => SLLSyntax.equiv(e1._1, e._1)).isEmpty) {
        ls += e
      }
    }
    ls.toList
  }

  private def nonTrivial(rb: RawRebuilding[Expr]): Boolean =
    rb match {
      case (Var(_), _)         => false
      case (_, m) if m.isEmpty => false
      case (e, sub) =>
        val bodyVars = SLLSyntax.vars(e)
        sub forall {
          case (_, v @ Var(_)) => bodyVars contains v
          case _               => true
        }
    }

  private def rebuild(top: Expr, e: Expr, sub: Subst[Expr]): List[RawRebuilding[Expr]] = {
    // rebuilding of arguments
    val rbs1 = e match {
      case FCall(n, xs) => rebuild1(top, xs, sub) map { case (ys, sub1) => (FCall(n, ys), sub1) }
      case GCall(n, xs) => rebuild1(top, xs, sub) map { case (ys, sub1) => (GCall(n, ys), sub1) }
      case Ctr(n, xs)   => rebuild1(top, xs, sub) map { case (ys, sub1) => (Ctr(n, ys), sub1) }
      case _            => List((e, sub))
    }
    // new rebuilding 
    val rbs2 = {
      val fn = newName(e, top)
      List((Var(fn), sub + (fn -> e)))
    }
    rbs2 ++ rbs1 
  }

  private def rebuild1(top: Expr, es: List[Expr], sub: Subst[Expr]): List[(List[Expr], Subst[Expr])] = {
    es.foldRight((List[Expr](), sub) :: Nil) { (e, acc) =>
      for { (es1, sub) <- acc; (t, sub1) <- rebuild(top, e, sub) } yield (t :: es1, sub1)
    }
  }

  // here we use top expression like #hash to prevent name collisions in the future
  // nice solution is welcome
  private def newName(e: Expr, top: Expr): Name =
    "gen/" + e.toString() + "[" + top.toString() + "]"
}