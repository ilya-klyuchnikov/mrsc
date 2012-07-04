package mrsc.pfp

import mrsc.core._
import NamelessSyntax._

sealed trait Binding
case class DefBinding(term: Term, after: Term) extends Binding
case class TermBinding(term: Term) extends Binding

// Used only by Residuator
// We put in this Residuation context new bindings and free variables (which becomes bound variables).
case class ResContext(l: List[Binding] = List()) {
  def addVar(t: Term): ResContext = ResContext(TermBinding(t) :: l)
  def addBinding(bind: Binding): ResContext = ResContext(bind :: l)
  def addBindings(binds: List[Binding]): ResContext = binds.foldLeft(this)(_.addBinding(_))
  def indexForTerm(t: Term) = l.indexWhere {
    case DefBinding(t1, _) => renaming(t, t1)
    case TermBinding(t1)   => t == t1
  }
  def getBinding(i: Int): Binding = l(i) match {
    case DefBinding(t1, t2) => DefBinding(t1, termShift(i, t2))
    case TermBinding(t)     => TermBinding(termShift(l.size - i - 1, t))
  }
}

case class Residuator(val g: TGraph[MetaTerm, Label]) {

  lazy val result: Term = fold(g.root, ResContext())

  def fold(node: TNode[MetaTerm, Label], ctx: ResContext): Term = node.conf match {
    case conf: Term =>
      node.base match {
        case None if g.leaves.exists(_.base == Some(node.tPath)) =>
          // TODO: parameterize it for lambda dropping!!
          val fvars = freeVars(conf)
          val app: Term = (BVar(0) :: fvars).reduceLeft(App)
          val absBody = {
            val extCtx = (ctx.addBinding(DefBinding(conf, app)) /: fvars)(_.addVar(_))
            val rawBody = construct(node, extCtx)
            val subst: Subst = freeVars(rawBody).map { fv => fv -> BVar(extCtx.indexForTerm(fv)) }.toMap
            applySubst(rawBody, subst)
          }
          val abs = (Abs(absBody) /: fvars) { (a, _) => Abs(a) }
          Let(Fix(abs), app)
        case None =>
          construct(node, ctx)
        case Some(_) =>
          val i = ctx.indexForTerm(conf)
          val DefBinding(t1, t2) = ctx.getBinding(i)
          val Some(renaming) = findSubst(t1, conf)
          applySubst(t2, renaming)
      }
    case _ => construct(node, ctx)
  }

  def construct(node: TNode[MetaTerm, Label], ctx: ResContext): Term =
    node.outs match {
      case TEdge(n1, DecomposeLabel(comp)) :: _ =>
        val subnodes = node.outs map { case TEdge(n, _) => n }
        val foldedParts = subnodes map { fold(_, ctx) }
        comp(foldedParts)
      case TEdge(n1, CaseBranchLabel(sel, _, _)) :: _ =>
        val bs1 = for (TEdge(n, CaseBranchLabel(_, ptr, ctr)) <- node.outs) yield {
          // valuable for shifting previous exprs in context
          val extCtx = ctx.addBindings(ctr.args.map(TermBinding(_)))
          val rawFolded = fold(n, extCtx)
          val subst: Subst = freeVars(ctr).map { v => v -> BVar(extCtx.indexForTerm(v)) }.toMap
          val folded = applySubst(rawFolded, subst)
          (ptr, folded)
        }
        Case(sel, bs1)
      case TEdge(n1, UnfoldLabel) :: Nil =>
        fold(n1, ctx)
      case TEdge(n1, TransientLabel) :: Nil =>
        fold(n1, ctx)
      case List() =>
        node.conf match {
          case t: Term => t
          case c       => sys.error("unexpected " + c)
        }
    }
}
