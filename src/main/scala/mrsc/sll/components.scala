package mrsc.sll

import mrsc._
import Decomposition._
import SLLExpressions._

trait SLLDriving {
  val program: Program

  def drive(pState: PState[Expr, SubStepInfo, Extra]) = {
    val driveStep: MStep[Expr, SubStepInfo, Extra] = if (tryComplete.isDefinedAt(pState)) {
      tryComplete(pState)
    } else {
      pureDrive(pState)
    }
    List(driveStep)
  }

  private def pureDrive(ps: PState[Expr, SubStepInfo, Extra]): MStep[Expr, SubStepInfo, Extra] = {
    MAddForest(drive_(ps))
  }

  def drive_(ps: PState[Expr, SubStepInfo, Extra]): List[SubStep[Expr, SubStepInfo, Extra]] =
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

  val tryComplete: PartialFunction[PState[Expr, SubStepInfo, Extra], MStep[Expr, SubStepInfo, Extra]] = {
    case PState(CoNode(_, _, Edge(_, StopStep), _, _), _) => MMakeLeaf
  }

  private def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}

// fold only into ancestors
trait SLLFolding[D, E] {

  def fold(ps: PState[Expr, D, E]): List[Path] =
    ps.node.ancestors.filter { renamingFilter(ps.node) } map { _.path }

  private def renamingFilter(leaf: CoNode[Expr, _, _])(n: CoNode[Expr, _, _]) =
    !n.conf.isInstanceOf[Var] && SLLExpressions.renaming(leaf.conf, n.conf)
}

trait SLLWhistle {
  val whistle: Whistle
  // small precularity on completion
  def blame(pState: PState[Expr, SubStepInfo, Extra]): Blaming[Expr, SubStepInfo, Extra] = {
    val inStep = if (pState.node.in != null) pState.node.in.driveInfo else null
    inStep match {
      case StopStep => Blaming(None, Whistle.OK)
      case _ => whistle.blame(pState) match {
        case None => Blaming(None, Whistle.OK)
        case s @ Some(_) => Blaming(s, Whistle.Warning)
      }
    }
  }
}

trait SLLRebuildings {
  def msg(conf: Expr, wrt: Expr): Expr = {
    println("msg requested")
    println(conf)
    println(wrt)
    msgToLet(conf, MSG.msg(conf, wrt))
  }

  def gens(conf: Expr): List[Expr] =
    SLLGeneralizations.gens(conf)

  private def msgToLet(ce: Expr, g: Gen) =
    if (renaming(g.t, ce) || g.t.isInstanceOf[Var]) {
      split(ce)
    } else {
      Let(g.t, g.m1.toList)
    }

  private def split(e: Expr): Expr =
    e match {
      case Ctr(name, args) => {
        val vs = args map freshVar
        Let(Ctr(name, vs), vs zip args)
      }
      case FCall(name, args) => {
        val vs = args map freshVar
        Let(FCall(name, vs), vs zip args)
      }
      case GCall(name, args) => {
        val vs = args map freshVar
        Let(GCall(name, vs), vs zip args)
      }
    }
}

