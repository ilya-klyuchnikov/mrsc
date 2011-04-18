package mrsc.sll

object NSLLExpressions {
  def nSubst(e: NExpr, m: Map[NVar, NExpr]): NExpr = e match {
    case v: NVar =>
      m.getOrElse(v, v)
    case NCtr(n, args) =>
      NCtr(n, args map { nSubst(_, m) })
    case NCall(n, args) =>
      NCall(n, args map { nSubst(_, m) })
    case NCase(sel, bs) =>
      NCase(nSubst(sel, m), bs map { case (p, e) => (p, nSubst(e, m -- p.args)) })
    case NLet(n, NFun(_, args, e), e1) =>
      NLet(n, NFun(n, args, nSubst(e, m -- args)), nSubst(e1, m))
  }

  def convert(t: Expr): NExpr = t match {
    case Var(n) => NVar(n)
    case Ctr(n, args) => NCtr(n, args map convert)
    case FCall(n, args) => NCall(n, args map convert)
    case GCall(n, args) => NCall(n, args map convert)
  }

  def convert(p: Pat): NPat =
    NPat(p.name, p.args map { v => NVar(v.name) })

  def canon(e: NExpr): NExpr = null

  def fixNames(e: NExpr): NExpr = {
    var v = 0
    def fv(x: Any = null): NVar = {
      v = v + 1
      NVar("v." + v)
    }

    def fixBoundVars(e: NExpr, m: Map[NVar, NVar]): NExpr = e match {
      case v: NVar => m.getOrElse(v, v)
      case NCtr(n, args) => NCtr(n, args map { fixBoundVars(_, m) })
      case NCall(n, args) => NCall(n, args map { fixBoundVars(_, m) })
      case NLet(n, NFun(_, args, e), e1) => {
        val freshArgs = args map fv
        val m1 = m ++ (args zip freshArgs)
        NLet(n, NFun(n, freshArgs, fixBoundVars(e, m1)), fixBoundVars(e1, m))
      }
      case NCase(sel, bs) => {
        val fSel = fixBoundVars(sel, m)
        val fBs = bs map {
          case (NPat(n, args), e) =>
            val fArgs = args map fv
            val m1 = m ++ (args zip fArgs)
            (NPat(n, fArgs), fixBoundVars(e, m1))
        }
        NCase(fSel, fBs)
      }
    }

    var f = 0
    def ff(x: Any = null): String = {
      f = f + 1
      "f." + f
    }

    def fixFs(e: NExpr, m: Map[String, String]): NExpr = e match {
      case v: NVar => v
      case NCtr(n, args) => NCtr(n, args map { fixFs(_, m) })
      case NCall(n, args) => NCall(m(n), args map { fixFs(_, m) })
      case NLet(n, NFun(_, args, e), e1) => {
        val fn = ff()
        val m1 = m + (n -> fn)
        NLet(fn, NFun(fn, args, fixFs(e, m1)), fixFs(e1, m1))
      }
      case NCase(sel, bs) => {
        val fSel = fixFs(sel, m)
        val fBs = bs map { case (p, e) => (p, fixFs(e, m)) }
        NCase(fSel, fBs)
      }
    }

    fixFs(fixBoundVars(e, Map()), Map())
  }
}