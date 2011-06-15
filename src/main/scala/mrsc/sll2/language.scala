package mrsc.sll

import mrsc._
import mrsc.sll.Decomposition._
import mrsc.sll.SLLExpressions._

object SLLSyntax extends Syntax[Expr] {

  override val instance: PartialOrdering[Expr] = new SimplePartialOrdering[Expr] {
    override def lteq(x: Expr, y: Expr) = SLLExpressions.inst(x, y)
  }

  override def subst(c: Expr, sub: Subst[Expr]): Expr = {
    val sllSub = sub map { kv => ((Var(kv._1), kv._2)) }
    SLLExpressions.subst(c, sllSub)
  }

  override def rebuildings(e: Expr): List[Rebuilding[Expr]] =
    SLLGeneralizations.gens2(e)

  override def rebuilding2Configuration(rebuilding: Rebuilding[Expr]): Expr = {
    val (e, sub) = rebuilding
    val bindings = sub.toList map { case (n, e) => (Var(n), e) }
    Let(e, bindings)
  }

  override def findSubst(from: Expr, to: Expr): Option[Subst[Expr]] = {
    val sllSub = SLLExpressions.findSubst(from, to)
    if (sllSub == null) {
      None
    } else {
      val sub = sllSub map { case (k, v) => (k.name, v) }
      Some(sub)
    }
  }

}

object SLLTermination extends Termination[Expr] {

  override val embedding = new SimplePartialOrdering[Expr] {
    override def lteq(x: Expr, y: Expr) = HE.heByCoupling(x, y)
  }

}

trait SLLMetaEvaluator extends Semantics[Expr] {
  val program: Program

  override def eval(conf: Expr): DriveStep[Expr] =
    decompose(conf) match {

      case ObservableVar(v) =>
        StopDriveStep

      case ObservableCtr(Ctr(cn, args)) =>
        val compose = { parts: List[Expr] =>
          Ctr(cn, parts)
        }
        DecomposeDriveStep(compose, args)

      case DecLet(Let(term, bs)) =>
        val compose = { parts: List[Expr] =>
          val in :: binds = parts
          val sub = (bs.map { p => Var(p._1.name) } zip binds).toMap
          subst(in, sub)
        }
        val parts = term :: (bs map (_._2))
        DecomposeDriveStep(compose, parts)

      case context @ Context(RedexFCall(FCall(name, args))) =>
        val fReduced = subst(program.f(name).term, Map(program.f(name).args.zip(args): _*))
        val nExpr = context.replaceRedex(fReduced)
        TransientDriveStep(nExpr)

      case context @ Context(RedexGCallCtr(GCall(name, args), Ctr(cname, cargs))) =>
        val g = program.g(name, cname)
        val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (cargs ::: args.tail): _*))
        val nExpr = context.replaceRedex(gReduced)
        TransientDriveStep(nExpr)

      case context @ Context(RedexGCallVar(GCall(name, args), v)) =>
        val cases = program.gs(name) map { g =>
          val fp = freshPat(g.p)
          val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (fp.args ::: args.tail): _*))
          val ctr : Expr = Ctr(fp.name, fp.args)
          val info = Map(v -> ctr)
          val contraction = Contraction(v.name, ctr)
          val driven = subst(context.replaceRedex(gReduced), info)
          (contraction, driven)
        }
        VariantsDriveStep(cases)

    }
  
  override def isReducible(e: Expr): Boolean = e match {
    case Var(_) => false
    case Ctr(_, Nil) => false
    case _ => true
  }

  private def freshPat(p: Pat) = Ctr(p.name, p.args map freshVar)
}