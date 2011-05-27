package mrsc.sll

import mrsc._
import Decomposition._
import SLLExpressions._

import Signal._

trait SLLDriving {
  val program: Program

  def isLeaf(pState: SLLState) = pState.node.conf match {
    case Var(_) => true
    case Ctr(_, Nil) => true
    case _ => false
  }

  def drive(ps: SLLState): SLLStep =
    Forest(drive_(ps.node.conf))

  private def drive_(conf: Expr): List[SubStep[Expr, SubStepInfo[Expr], Extra]] =
    decompose(conf) match {

      case DecLet(Let(term, bs)) =>
        val compose = { parts: List[NExpr] =>
          val sub = (bs.map { p => NVar(p._1.name) } zip parts.tail).toMap
          val body = parts.head
          NSLLExpressions.nSubst(body, sub)
        }
        val stepInfo = DecomposeStep(compose)
        SubStep(term, stepInfo, NoExtra) :: bs.map {
          case (k, v) => SubStep(v, stepInfo, NoExtra)
        }

      case ObservableCtr(Ctr(cn, args)) =>
        val compose = { parts: List[NExpr] =>
          NCtr(cn, parts)
        }
        val stepInfo = DecomposeStep(compose)
        args map { a => SubStep(a, stepInfo, NoExtra) }

      case ObservableVar(v) =>
        List()

      case context @ Context(RedexFCall(FCall(name, args))) =>
        val fReduced = subst(program.f(name).term, Map(program.f(name).args.zip(args): _*))
        val nExpr = context.replaceRedex(fReduced)
        List(SubStep(nExpr, TransientStep, NoExtra))

      case context @ Context(RedexGCallCtr(GCall(name, args), Ctr(cname, cargs))) =>
        val g = program.g(name, cname)
        val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (cargs ::: args.tail): _*))
        val nExpr = context.replaceRedex(gReduced)
        List(new SubStep(nExpr, TransientStep, NoExtra))

      case context @ Context(RedexGCallVar(GCall(name, args), v)) =>
        val branches = program.gs(name) map { g =>
          val fp = freshPat(g.p)
          val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (fp.args ::: args.tail): _*))
          val info = Map(v -> Ctr(fp.name, fp.args))
          val driven = subst(context.replaceRedex(gReduced), info)
          new SubStep(driven, VariantBranchStep(Contraction(v.name, fp)), NoExtra)
        }
        branches

    }

  private def freshPat(p: Pat) = Ctr(p.name, p.args map freshVar)
}

// fold only into ancestors
trait SLLFolding[D, E] {

  import StepKind._
  def fold(ps: PState[Expr, SubStepInfo[Expr], Extra]): Option[Path] =
    ps.node.ancestors.find { renamingFilter(ps.node) } map { _.path }

  private def renamingFilter(leaf: CoNode[Expr, _, _])(n: CoNode[Expr, _, _]) =
    SLLExpressions.renaming(leaf.conf, n.conf)
}

trait SLLWhistle {
  val whistle: Whistle

  def blame(pState: PState[Expr, SubStepInfo[Expr], Extra]): Blaming[Expr, SubStepInfo[Expr], Extra] =
    whistle.blame(pState) match {
      case None => Blaming(None, Signal.OK)
      case s @ Some(_) => Blaming(s, Signal.Warning)
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

  protected def split(e: Expr): Expr =
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

