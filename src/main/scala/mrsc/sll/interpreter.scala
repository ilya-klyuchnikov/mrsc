package mrsc.sll

import Decomposition._
import SLLExpressions._

object SLLInterpreter {
  def eval(task: SLLTask): Expr = {
    val int = new SLLInterpreter(task.program)
    int.eval(task.target)
  }
}

private class SLLInterpreter(program: Program) {

  private def eval(t: Expr): Expr = lazyEval(t) match {
    case Ctr(name, args) => Ctr(name, args.map(eval))
    case x => throw new Exception("Internal Error: lazy eval returns " + x)
  }

  private def lazyEval(term: Expr): Expr = {
    var t = term
    while (decompose(t) match { case _: Context => true; case _ => false }) { t = baseLazyEval(t) }
    t
  }

  private def baseLazyEval(t: Expr): Expr = decompose(t) match {
    case ObservableCtr(ctr) =>
      ctr
    case ObservableVar(v) =>
      v
    case context @ Context(RedexFCall(FCall(name, args))) =>
      val fReduced = subst(program.f(name).term, Map(program.f(name).args.zip(args): _*))
      context.replaceRedex(fReduced)
    case context @ Context(RedexGCallCtr(GCall(name, args), Ctr(cname, cargs))) =>
      val g = program.g(name, cname)
      val gReduced = subst(g.term, Map((g.p.args ::: g.args) zip (cargs ::: args.tail): _*))
      context.replaceRedex(gReduced)
    case _ =>
      throw new Error("unexpected expression: " + t)
  }
}