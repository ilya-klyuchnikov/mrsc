package mrsc.sll

import mrsc._
// SLL = Simple Lazy Language, first-order with simple pattern matching.
// The intended semantics is the usual normal order reduction of the lambda calculus.

sealed trait Expr {
  def size(): Int
}

case class Var(name: String) extends Expr {
  val size = 1
  override def toString = name
}

case class Ctr(name: String, args: List[Expr]) extends Expr {
  lazy val size = 1 + (args map { _.size }).sum
  override def toString = name + args.mkString("(", ", ", ")")
}
case class FCall(name: String, args: List[Expr]) extends Expr {
  lazy val size = 1 + (args map { _.size }).sum
  override def toString = name + args.mkString("(", ", ", ")")

}
case class GCall(name: String, args: List[Expr]) extends Expr {
  lazy val size = 1 + (args map { _.size }).sum
  override def toString = name + args.mkString("(", ", ", ")")
}

case class Let(term: Expr, bindings: List[(Var, Expr)]) extends Expr {
  lazy val size = 1 + (bindings map { _._2.size }).sum
  override def toString = "let " + (bindings map {case (x, y) => x + "="+y} mkString(", ")) + " in " + term
}

case class Pat(name: String, args: List[Var]) {
  override def toString = name + args.mkString("(", ", ", ")")
}

abstract class Def { def name: String }
case class FFun(name: String, args: List[Var], term: Expr) extends Def {
  override def toString = name + args.mkString("(", ", ", ")") + " = " + term + ";"
}
case class GFun(name: String, p: Pat, args: List[Var], term: Expr) extends Def {
  override def toString = name + (p :: args).mkString("(", ", ", ")") + " = " + term + ";"
}

case class Program(defs: List[Def]) {
  val f = (defs :\ (Map[String, FFun]())) { case (x: FFun, m) => m + (x.name -> x); case (_, m) => m }
  val g = (defs :\ (Map[(String, String), GFun]())) { case (x: GFun, m) => m + ((x.name, x.p.name) -> x); case (_, m) => m }
  val gs = (defs :\ Map[String, List[GFun]]().withDefaultValue(Nil)) { case (x: GFun, m) => m + (x.name -> (x :: m(x.name))); case (_, m) => m }
  override def toString = defs.mkString("\n")
}

case class SLLTask(target: Expr, program: Program)