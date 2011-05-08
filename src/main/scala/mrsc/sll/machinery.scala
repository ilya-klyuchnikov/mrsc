package mrsc.sll

import mrsc._
import Decomposition._
import SLLExpressions._

trait SLLDriving {
  val program: Program

  def drive(ps: PState[Expr, SubStepInfo, Extra]): List[SubStep[Expr, SubStepInfo, Extra]] =
    decompose(ps.node.conf) match {

      case DecLet(Let(term, bs)) =>
        SubStep(term, LetBodyStep, DummyExtra) :: bs.map {
          case (k, v) => SubStep(v, LetPartStep(k), DummyExtra)
        }

      case ObservableCtr(c @ Ctr(_, Nil)) =>
        List(SubStep(c, StopStep, DummyExtra))

      case ObservableCtr(Ctr(_, args)) =>
        args map { a => SubStep(a, CtrArgStep, DummyExtra) }

      case ObservableVar(v) =>
        List(SubStep(v, StopStep, DummyExtra))

      case context @ Context(RedexFCall(FCall(name, args))) =>
        val fReduced = subst(program.f(name).term, Map(program.f(name).args.zip(args): _*))
        val nExpr = context.replaceRedex(fReduced)
        List(SubStep(nExpr, TransientStep, DummyExtra))

      case context @ Context(RedexGCallCtr(GCall(name, args), Ctr(cname, cargs))) =>
        val g = program.g(name, cname)
        val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (cargs ::: args.tail): _*))
        val nExpr = context.replaceRedex(gReduced)
        List(new SubStep(nExpr, TransientStep, DummyExtra))

      case context @ Context(RedexGCallVar(GCall(name, args), v)) =>
        val branches = program.gs(name) map { g =>
          val fp = freshPat(g.p)
          val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (fp.args ::: args.tail): _*))
          val info = Map(v -> Ctr(fp.name, fp.args))
          val driven = subst(context.replaceRedex(gReduced), info)
          new SubStep(driven, VariantBranchStep(Contraction(v, fp)), DummyExtra)
        }
        val sel = new SubStep(v, VariantSelectorStep, DummyExtra)
        sel :: branches

    }

  val tryComplete: PartialFunction[PState[Expr, SubStepInfo, Extra], List[MStep[Expr, SubStepInfo, Extra]]] = {
    case PState(CoNode(_, _, Edge(_, StopStep), _, _), _) => List(MMakeLeaf)
  }

  private def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}