package mrsc.sll

import SLLExpressions._

case class Rule(from: Expr, to: Expr)

// proof of concept
class Speculator(val program: Program) {
  val rules: List[Rule] = program.defs map {
    case FFun(name, args, body) =>
      Rule(FCall(name, args), body)

    case GFun(name, Pat(cname, cargs), args, body) =>
      Rule(GCall(name, Ctr(cname, cargs) :: args), body)
  }

  def speculate(expr: Expr): List[Expr] =
    rules map { speculateRule(_)(expr) } filter { _.isRight } map { _.merge }

  def speculateRule(rule: Rule)(expr: Expr): Either[Expr, Expr] = expr match {
    case Ctr(n, args) => {
      val args1 = args map { speculateRule(rule) }
      if (args1.exists { _.isRight }) {
        val args2 = args1 map { _.merge }
        val in = Ctr(n, args2)
        Right(in)
      } else {
        tryRule(rule, expr)
      }
    }
    case FCall(n, args) => {
      val args1 = args map { speculateRule(rule) }
      if (args1.exists { _.isRight }) {
        val args2 = args1 map { _.merge }
        val in = FCall(n, args2)
        Right(in)
      } else {
        tryRule(rule, expr)
      }
    }
    case GCall(n, args) => {
      val args1 = args map { speculateRule(rule) }
      if (args1.exists { _.isRight }) {
        val args2 = args1 map { _.merge }
        val in = GCall(n, args2)
        Right(in)
      } else {
        tryRule(rule, expr)
      }
    }
    case _ => Left(expr)
  }

  def tryRule(rule: Rule, expr: Expr): Either[Expr, Expr] = {
    val sub = findSubst(rule.from, expr)
    Either.cond(sub != null, subst(rule.to, sub), expr)
  }
}