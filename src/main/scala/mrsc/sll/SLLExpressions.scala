package mrsc.sll

import mrsc._

// FIXME: Here we assume that there are no free variables 
// in bodies of let-expressions and where-expressions.
object SLLExpressions {

  def subst(term: Expr, m: Subst[Expr]): Expr = term match {
    case v@Var(n) =>
      m.getOrElse(n, v)
    case Ctr(name, args) =>
      Ctr(name, args map { subst(_, m) })
    case FCall(name, args) =>
      FCall(name, args map { subst(_, m) })
    case GCall(name, args) =>
      GCall(name, args map { subst(_, m) })
    case Where(e, defs) =>
      Where(subst(e, m), defs map { subst(_, m) })
    case Let(e, bs) =>
      Let(subst(e, m), bs)
  }

  private def subst(deff: Def, m: Subst[Expr]): Def = deff match {
    case FFun(n, args, body) => 
      FFun(n, args, subst(body, m -- args))
    case GFun(n, Pat(pn, pargs), args, body) => 
      GFun(n, Pat(pn, pargs), args, subst(body, m -- pargs -- args))
  }

  def renaming(t1: Expr, t2: Expr): Boolean = t1.size == t2.size && inst(t1, t2) && inst(t2, t1)

  def inst(t1: Expr, t2: Expr): Boolean = (t1.size <= t2.size) && (findSubst(t1, t2) != null)

  def findSubst(t1: Expr, t2: Expr): Subst[Expr] = {
    val map = scala.collection.mutable.Map[Name, Expr]()
    def walk(t1: Expr, t2: Expr): Boolean = (t1, t2) match {
      case (v1: Var, _) => map.getOrElse(v1.name, t2) == (map += (v1.name -> t2))(v1.name)
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

  
  // canonizes names of variables 
  def fixNames(e: Expr): Expr = {
    var fmap: Map[String, String] = Map()
    var v = 0
    var f = 0
    // fresh var name
    def fv(x: Any = null): String = {
      v = v + 1
      "v." + v
    }
    // fresh function name
    def ff(x: Any = null): String = {
      f = f + 1
      "f." + f
    }

    
    def fixBoundVars(e: Expr, m: Map[Name, Name]): Expr = e match {
      case Var(n) => Var(m.getOrElse(n, n))
      case Ctr(n, args) => Ctr(n, args map { fixBoundVars(_, m) })
      case FCall(n, args) => FCall(n, args map { fixBoundVars(_, m) })
      case GCall(n, args) => GCall(n, args map { fixBoundVars(_, m) })
      case Where(e, defs) => {
        val defs1 = defs map { fixBoundVarsInDef(_, m) }
        val e1 = fixBoundVars(e, m)
        Where(e1, defs1)
      }
    }

    // gives ordered names to bound variables: v1, v2, ..
    // so, it performs some kind of "canonization"
    def fixBoundVarsInDef(deff: Def, m: Map[Name, Name]): Def = deff match {
      case FFun(name, args, body) => {
        val args1 = args map fv
        val m1 = m ++ (args zip args1)
        val body1 = fixBoundVars(body, m1)
        FFun(name, args1, body1)
      }
      case GFun(name, Pat(n, pargs), args, body) => {
        val pargs1 = pargs map fv
        val args1 = args map fv
        val m1 = m ++ (args zip args1) ++ (pargs zip pargs1)
        val body1 = fixBoundVars(body, m1)
        GFun(name, Pat(n, pargs1), args1, body1)
      }
    }

    def fixDefFs(deff: Def): Def = deff match {
      case FFun(n, args, body) => fmap.get(n) match {
        case Some(n1) =>
          FFun(n1, args, fixFs(body))
        case None =>
          val n1 = ff(n)
          fmap = fmap + (n -> n1)
          FFun(n1, args, fixFs(body))
      }
      case GFun(n, pat, args, body) => fmap.get(n) match {
        case Some(n1) =>
          GFun(n1, pat, args, fixFs(body))
        case None =>
          val n1 = ff(n)
          fmap = fmap + (n -> n1)
          GFun(n1, pat, args, fixFs(body))
      }

    }

    def fixFs(e: Expr): Expr = e match {
      case v: Var => v
      case Ctr(n, args) => Ctr(n, args map fixFs)
      case FCall(n, args) => FCall(fmap(n), args map fixFs)
      case GCall(n, args) => GCall(fmap(n), args map fixFs)
      case Where(e, defs) => {
        val defs1 = defs map fixDefFs
        val e1 = fixFs(e)
        Where(e1, defs1)
      }
    }

    fixFs(fixBoundVars(e, Map()))
  }

  def expr2Task(e: Expr): SLLTask = {
    val (e1, defs) = lift(e)

    def cmp(n1: String, n2: String): Boolean = {
      if (n1.length < n2.length) {
        true
      } else if (n1.length > n2.length) {
        false
      } else {
        n1 < n2
      }
    }

    val defs1 = defs sortWith { (d1, d2) =>
      if (cmp(d1.name, d2.name) && cmp(d2.name, d1.name)) {
        val GFun(n1, Pat(pn1, _), _, _) = d1
        val GFun(n2, Pat(pn2, _), _, _) = d2
        pn1 < pn2
      } else cmp(d1.name, d2.name)
    }

    SLLTask(e1, Program(defs1))
  }

  private def lift(e: Expr): (Expr, List[Def]) = e match {
    case v: Var =>
      (v, Nil)

    case Ctr(n, args) =>
      var args1 = List[Expr]()
      var defs = List[Def]()
      for (arg <- args) {
        val (arg1, defs1) = lift(arg)
        args1 = args1 ++ List(arg1)
        defs = defs ++ defs1
      }
      (Ctr(n, args1), defs)

    case FCall(n, args) =>
      var args1 = List[Expr]()
      var defs = List[Def]()
      for (arg <- args) {
        val (arg1, defs1) = lift(arg)
        args1 = args1 ++ List(arg1)
        defs = defs ++ defs1
      }
      (FCall(n, args1), defs)

    case GCall(n, args) =>
      var args1 = List[Expr]()
      var defs = List[Def]()
      for (arg <- args) {
        val (arg1, defs1) = lift(arg)
        args1 = args1 ++ List(arg1)
        defs = defs ++ defs1
      }
      (GCall(n, args1), defs)

    case Where(e, defs) =>
      var newDefs = List[Def]()
      for (deff <- defs) deff match {
        case FFun(n, vs, body) =>
          val (body1, defs1) = lift(body)
          val def1 = FFun(n, vs, body1)
          newDefs = newDefs ++ defs1 ++ List(def1)
        case GFun(n, p, vs, body) =>
          val (body1, defs1) = lift(body)
          val def1 = GFun(n, p, vs, body1)
          newDefs = newDefs ++ defs1 ++ List(def1)
      }
      val (e1, defs1) = lift(e)
      newDefs = newDefs ++ defs1
      (e1, newDefs)
  }
}