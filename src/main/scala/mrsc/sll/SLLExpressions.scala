package mrsc.sll

// FIXME: Here we assume that in bodies of let expressions and
// where expressions no free variables
object SLLExpressions {

  def subst(term: Expr, m: Map[Var, Expr]): Expr = term match {
    case v: Var =>
      m.getOrElse(v, v)
    case Ctr(name, args) =>
      Ctr(name, args map { subst(_, m) })
    case FCall(name, args) =>
      FCall(name, args map { subst(_, m) })
    case GCall(name, args) =>
      GCall(name, args map { subst(_, m) })
    case Where(e, defs) =>
      Where(subst(e, m), defs)
    case Let(e, bs) =>
      Let(subst(e, m), bs)
  }

  def renaming(t1: Expr, t2: Expr): Boolean = t1.size == t2.size && inst(t1, t2) && inst(t2, t1)

  def inst(t1: Expr, t2: Expr): Boolean = (t1.size <= t2.size) && (findSubst(t1, t2) != null)

  def findSubst(t1: Expr, t2: Expr): Map[Var, Expr] = {
    val map = scala.collection.mutable.Map[Var, Expr]()
    def walk(t1: Expr, t2: Expr): Boolean = (t1, t2) match {
      case (v1: Var, _) => map.getOrElse(v1, t2) == (map += (v1 -> t2))(v1)
      case (Ctr(n1, args1), Ctr(n2, args2)) => n1 == n2 && (args1, args2).zipped.forall(walk)
      case (FCall(n1, args1), FCall(n2, args2)) => n1 == n2 && (args1, args2).zipped.forall(walk)
      case (GCall(n1, args1), GCall(n2, args2)) => n1 == n2 && (args1, args2).zipped.forall(walk)
      case _ => false
    }
    if (walk(t1, t2)) Map(map.toList: _*).filter { case (k, v) => k != v } else null
  }

  def vars(t: Expr): List[Var] = t match {
    case v: Var =>
      List(v)
    case Ctr(_, args) =>
      (List[Var]() /: args) { (vs, exp) => vs ++ (vars(exp) filterNot (vs contains)) }
    case FCall(_, args) =>
      (List[Var]() /: args) { (vs, exp) => vs ++ (vars(exp) filterNot (vs contains)) }
    case GCall(_, args) =>
      (List[Var]() /: args) { (vs, exp) => vs ++ (vars(exp) filterNot (vs contains)) }
    case Let(e, _) =>
      vars(e)
    case Where(e, _) =>
      vars(e)
  }

  private var i: Long = 0;
  def freshVar(x: AnyRef = null) = { i += 1; Var("v" + i) };

}