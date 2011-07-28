package mrsc.sll

import mrsc._
import mrsc.sll.Decomposition._
import mrsc.sll.SLLExpressions._

trait SLLSyntax extends Syntax[Expr] {

  override val instance: PartialOrdering[Expr] = new SimplePartialOrdering[Expr] {
    override def lteq(x: Expr, y: Expr) = SLLExpressions.inst(x, y)
  }

  override def subst(c: Expr, sub: Subst[Expr]): Expr = {
    SLLExpressions.subst(c, sub)
  }

  override def rawRebuildings(e: Expr): List[Rebuilding[Expr]] =
    SLLRebuilding.rebuildings(e)

  override def translate(rebuilding: Rebuilding[Expr]): Expr = {
    val (e, sub) = rebuilding
    Let(e, sub.toList)
  }

  override def findSubst(from: Expr, to: Expr): Option[Subst[Expr]] = {
    val sllSub = SLLExpressions.findSubst(from, to)
    if (sllSub == null) None else Some(sllSub)
  }

  override def size(e: Expr) = e.size

}

trait SLLSemantics extends OperationalSemantics[Expr] {
  val program: Program

  override def drive(conf: Expr): DriveStep[Expr] =
    decompose(conf) match {

      case ObservableVar(v) =>
        StopDriveStep

      case ObservableCtr(Ctr(cn, args)) =>
        DecomposeDriveStep({ Ctr(cn, _: List[Expr]) }, args)

      case DecLet(Let(term, bs)) =>
        val (names, es) = bs.unzip
        val compose = { parts: List[Expr] =>
          val in :: binds = parts
          val sub = (names zip binds).toMap
          subst(in, sub)
        }
        DecomposeDriveStep(compose, term :: es)

      case context @ Context(RedexFCall(FCall(name, args))) =>
        val FFun(_, fargs, body) = program.f(name)
        val fReduced = subst(body, (fargs zip args).toMap)
        val nExpr = context.replaceRedex(fReduced)
        TransientDriveStep(nExpr)

      case context @ Context(RedexGCallCtr(GCall(name, _ :: args), Ctr(cname, cargs))) =>
        val GFun(_, p, gargs, body) = program.g(name, cname)
        val gReduced = subst(body, ((p.args ++ gargs) zip (cargs ++ args)).toMap)
        val nExpr = context.replaceRedex(gReduced)
        TransientDriveStep(nExpr)

      case context @ Context(RedexGCallVar(GCall(name, _ :: args), v)) =>
        val cases = program.gs(name) map {
          case GFun(_, p, gargs, body) =>
            val cargs = p.args map freshVar
            val gReduced = subst(body, ((p.args ++ gargs) zip (cargs ++ args)).toMap)
            val contraction = Contraction(v.name, Ctr(p.name, cargs))
            val driven = subst(context.replaceRedex(gReduced), contraction.subst)
            (contraction, driven)
        }
        VariantsDriveStep(cases)

    }

  override def isDrivable(e: Expr): Boolean = e match {
    case Var(_) => false
    case Ctr(_, Nil) => false
    case _ => true
  }

}