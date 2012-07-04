package mrsc.pfp

// Terms are states of our LTS.
sealed trait MetaTerm
case class Rebuilding(t: Term, sub: Subst) extends MetaTerm

sealed trait Term extends MetaTerm
// Nameless bound variable coded by means of de-Bruijn indices.
// De-Bruijn indices are indexed from zero.
case class BVar(i: Int) extends Term {
  override def toString = i.toString
}
// Free variables are nameless.
// It is done in order to simplify generating
// of new variables.
case class FVar(i: Int) extends Term {
  override def toString = "<" + i + ">"
}
// Global variables are named for the same reasons.
case class GVar(n: String) extends Term {
  override def toString = n
}
// Lambda abstraction.
case class Abs(t: Term) extends Term {
  override def toString = "(\\" + t + ")"
}
// Application.
case class App(t1: Term, t2: Term) extends Term {
  override def toString = "(" + t1 + " " + t2 + ")"
}
// Simple let-expression. `v` is represented by `BVar(0)` in `in`.
case class Let(v: Term, in: Term) extends Term
// Fix point combinator.
// Term itself is represented as `BVar(0)` in `t`.
case class Fix(t: Term) extends Term
case class Ctr(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", ", ")")
}
case class Case(sel: Term, branches: List[Branch]) extends Term {
  override def toString = "case " + sel + " of " + branches.map(b => b._1 + " -> " + b._2).mkString("{", "; ", "}")
}
case class Ptr(name: String, args: List[String]) {
  override def toString = name + args.mkString("(", ", ", ")") 
}

// Labels or actions of our LTS.
// We put it on graph edges.
sealed trait Label
case object TransientLabel extends Label {
  override def toString = "->"
}
case object UnfoldLabel extends Label {
  override def toString = "->*"
}
case class CaseBranchLabel(sel: Term, ptr: Ptr, alt: Ctr) extends Label {
  override def toString = sel + " = " + alt
}
case class DecomposeLabel(compose: List[Term] => Term) extends Label {
  override def toString = ""
}
    