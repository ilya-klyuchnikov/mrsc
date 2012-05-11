package mrsc.pfp

import mrsc.core._

sealed trait Binding
case class DefBinding(term: Term, after: Term) extends Binding
case class TermBinding(term: Term) extends Binding

case class ResContext(l: List[Binding] = List()) {
  def addBinding(bind: Binding): ResContext = ResContext(bind :: l)
  def addBindings(binds: List[Binding]): ResContext = binds.foldLeft(this)(_.addBinding(_))
  def indexForTerm(t: Term) = l.indexWhere {
    case DefBinding(t1, _) => Syntax.renaming(t, t1)
    case TermBinding(t1)   => t == t1
  }

  def getBinding(i: Int): Binding = l(i) match {
    case DefBinding(t1, t2) =>
      val shift = i
      DefBinding(t1, Syntax.termShift(shift, t2))
    case TermBinding(t) =>
      val shift = l.size - i - 1
      TermBinding(Syntax.termShift(shift, t))
  }
}

case class Residuator(val g: TGraph[Term, DeforestStep]) {

  val result: Term = fold(g.root, ResContext())

  // this method tries either to create recursive definition,
  // or to make a recursive call.
  // otherwise, it passes control to construct
  def fold(node: TNode[Term, DeforestStep], ctx: ResContext): Term =
    node.base match {
      case None =>
        val base = g.leaves.exists(_.base == Some(node.tPath))
        if (base) {
          val conf = node.conf
          val fvars = Syntax.freeVars(node.conf)
          val app: Term = (BVar(0) :: fvars).reduceLeft(App)
          val bind = DefBinding(conf, app)
          var ctx1 = ctx
          ctx1 = ctx1.addBinding(bind)
          for (fv <- fvars) {
            ctx1 = ctx1.addBinding(TermBinding(fv))
          }

          var body = construct(node, ctx1)
          val fvars1 = Syntax.freeVars(body)
          for (fv <- fvars1) {
            val i = ctx1.indexForTerm(fv)
            body = Syntax.applySubst(body, Map(fv -> BVar(i)))
          }

          var abs = Abs(body)
          for (fv <- fvars) {
            abs = Abs(abs)
          }
          Let(Fix(abs), app)
        } else
          construct(node, ctx)
      case Some(_) =>
        val conf = node.conf
        val i = ctx.indexForTerm(conf)
        val DefBinding(t1, t2) = ctx.getBinding(i)
        val Some(renaming) = Syntax.findSubst(t1, conf)
        var res = Syntax.applySubst(t2, renaming)
        res
    }

  def construct(node: TNode[Term, DeforestStep], ctx: ResContext): Term = node.conf match {
    case Ctr(n, _) =>
      val args: List[Term] = node.outs.map { case TEdge(child, CtrArg) => fold(child, ctx) }
      Ctr(n, args)
    case v @ FVar(n) =>
      v
    case _ =>
      node.outs match {
        case bs @ (TEdge(n1, CaseBranch(sel, _, _)) :: _) =>
          val bs1 = for (TEdge(n, CaseBranch(_, ptr, ctr)) <- bs) yield {
            val ctx1 = ctx.addBindings(ctr.args.map(TermBinding(_)))
            var folded = fold(n, ctx1)
            val fvars = Syntax.freeVars(ctr)
            for (fv <- fvars) {
              val i = ctx1.indexForTerm(fv)
              folded = Syntax.applySubst(folded, Map(fv -> BVar(i)))
            }
            (ptr, folded)
          }
          Case(sel, bs1)
        case List(TEdge(n1, TransientStep)) =>
          fold(n1, ctx)
        case List() =>
          node.conf
      }
  }
}