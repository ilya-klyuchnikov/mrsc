package mrsc.sll

import mrsc._
import Decomposition._
import SLLExpressions._

class SLLMachine(p: Program, wh: Whistle) extends SingleMachine[Expr, Contraction] with MultiMachine[Expr, Contraction] {
  val name = wh.name

  def makeStep(ps: PState[Expr, Contraction]): Step[Expr, Contraction] =
    // TODO!! here may be different ways of folding!!!
    ps.node.ancestors.find { n => !n.configuration.isInstanceOf[Var] && SLLExpressions.renaming(ps.node.configuration, n.configuration) } match {
      case Some(n) =>
        MFold(n.path)
      case None => Decomposition.decompose(ps.node.configuration) match {
        case ObservableVar(v) =>
          MComplete
        case ObservableCtr(Ctr(_, Nil)) =>
          MComplete
        case _ =>
          val e = ps.node.configuration
          val driveStep = if (whistle(ps)) MForest(drivingStep(e)) else MPrune
          driveStep
      }
    }

  def makeSteps(ps: PState[Expr, Contraction]): List[Step[Expr, Contraction]] =
    ps.node.ancestors.filter { n => !n.configuration.isInstanceOf[Var] && SLLExpressions.renaming(ps.node.configuration, n.configuration) } match {
      case x if !x.isEmpty =>
        x map {n => MFold(n.path)}
      case _ => Decomposition.decompose(ps.node.configuration) match {
        case ObservableVar(v) =>
          List(MComplete)
        case ObservableCtr(Ctr(_, Nil)) =>
          List(MComplete)
        case _ =>
          val e = ps.node.configuration
          val driveStep =
            if (whistle(ps)) MForest(drivingStep(e)) else MPrune
          val notGen =
            (ps.node.parent != null) && (ps.node.parent.configuration match { case Let(_, _) => true; case _ => false })
          // we do not try to generalize if previous step was a generalization
          val genSteps =
            if (notGen) Nil else SLLGeneralizations.gens(e) map { e1 =>
              MReplace(e1, ps.node.info)
              //e1 => MForest(List(new SubStep(e1, null)))
            }
          driveStep :: genSteps
      }
    }

  // only simple whistle for now -- will continue
  def whistle(ps: PState[Expr, Contraction]): Boolean = wh.blame(ps).isEmpty

  def drivingStep(configuration: Expr): List[SubStep[Expr, Contraction]] = decompose(configuration) match {
    case DecLet(Let(term, bs)) =>
      new SubStep[Expr, Contraction](term, null) :: bs.map { case (_, v) => new SubStep[Expr, Contraction](v, null) }
    case ObservableCtr(Ctr(_, args)) => args map { a => new SubStep[Expr, Contraction](a, null) }
    case context @ Context(red) =>
      red match {
        case RedexFCall(FCall(name, args)) => {
          val fReduced = subst(p.f(name).term, Map(p.f(name).args.zip(args): _*))
          val nExpr = context.replaceRedex(fReduced)
          List(new SubStep(nExpr, null))
        }
        case RedexGCallCtr(GCall(name, args), Ctr(cname, cargs)) => {
          val g = p.g(name, cname)
          val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (cargs ::: args.tail): _*))
          val nExpr = context.replaceRedex(gReduced)
          List(new SubStep(nExpr, null))
        }
        case RedexGCallVar(GCall(name, args), v) => {
          p.gs(name) map { g =>
            val fp = freshPat(g.p)
            val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (fp.args ::: args.tail): _*))
            val info = Map(v -> Ctr(fp.name, fp.args))
            val driven = subst(context.replaceRedex(gReduced), info)
            new SubStep(driven, Contraction(v, fp))
          }
        }
      }
  }

  private def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}

class SLLMachine1(p: Program, wh: Whistle) extends SLLMachine(p, wh) {
  override def makeSteps(ps: PState[Expr, Contraction]): List[Step[Expr, Contraction]] =
    ps.completeNodes.find { n => SLLExpressions.renaming(ps.node.configuration, n.configuration) } match {
      case Some(n) =>
        List(MFold(n.path))
      case None => Decomposition.decompose(ps.node.configuration) match {
        case ObservableVar(v) =>
          List(MComplete)
        case ObservableCtr(Ctr(_, Nil)) =>
          List(MComplete)
        case _ =>
          val e = ps.node.configuration
          val accept = whistle(ps)
          val driveStep = 
            if (accept) MForest(drivingStep(e)) else MPrune
          val notGen =
            (ps.node.parent != null) && (ps.node.parent.configuration match { case Let(_, _) => true; case _ => false })
          // we do not try to generalize if previous step was a generalization
          val genSteps =
            if (notGen || (!accept)) Nil else SLLGeneralizations.gens(e) map { e1 =>
              MReplace(e1, ps.node.info)
              //e1 => MForest(List(new SubStep(e1, null)))
            }
          driveStep :: genSteps
      }
    }
}