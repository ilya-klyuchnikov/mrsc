package mrsc.sll

// should be rewritten in more transparent way
// for now we just AD-HOC generate and remove duplicates here 
object SLLGeneralizations {

  type Sub = List[(Var, Expr)]

  // trick #1: remove duplicates here
  //
  // trick #2: do not generalize let expressions and constructors
  // because a sub-expression may be generalized further
  def gens(e: Expr): List[Let] = e match {
    case Let(_, _) => Nil
    case Ctr(_, _) => Nil
    case _ => generalize(e, Nil).
      foldRight(List[(Expr, Sub)]()) { (elem, filtered) =>
        filtered.find { x => SLLExpressions.renaming(elem._1, x._1) } match { case None => elem :: filtered; case Some(_) => filtered }
      }.
      filter { !_._1.isInstanceOf[Var] }. // remove full abstraction
      filter { !_._2.isEmpty }. // remove identity
      map { case (t, sub) => Let(t, sub) }
  }

  private def generalize(e: Expr, sub: Sub): List[(Expr, Sub)] = {

    // generalizations of subcomponents
    val xs = e match {
      case FCall(n, args) if !args.isEmpty =>
        generalizeArgs(args) map { case (args1, sub1) => (FCall(n, args1), sub1 ++ sub) }
      case GCall(n, args) if !args.isEmpty =>
        generalizeArgs(args) map { case (args1, sub1) => (GCall(n, args1), sub1 ++ sub) }
      case Ctr(n, args) if !args.isEmpty =>
        generalizeArgs(args) map { case (args1, sub1) => (Ctr(n, args1), sub1 ++ sub) }
      case t => List((t, sub))
    }

    // a reference to an already defined binding, if any
    val ys: List[(Expr, Sub)] = sub find { _._2 == e } match {
      case None => Nil
      case Some((v, _)) => List((v, sub))
    }

    // the whole (sub)-expression is abstracted
    val ns: List[(Expr, Sub)] = {
      val fv = SLLExpressions.freshVar()
      List((fv, (fv, e) :: sub))
    }

    ys ++ ns ++ xs
  }

  // subtask: generalize elements and merge
  private def generalizeArgs(args: List[Expr]): List[(List[Expr], Sub)] =
    args.foldRight(List[(List[Expr], Sub)]((Nil, Nil))) { (arg, acc) =>
      for ((terms, sub) <- acc; (t, sub1) <- generalize(arg, sub)) yield (t :: terms, sub1)
    }

}