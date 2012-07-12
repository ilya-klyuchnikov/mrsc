package mrsc.pfp

sealed trait MetaTerm
case class Rebuilding(t: Term, sub: Subst) extends MetaTerm

sealed trait Term extends MetaTerm {
  def size: Int
}
case class BVar(i: Int) extends Term {
  override def toString = i.toString
  override lazy val size = 1
}
case class FVar(i: Int) extends Term {
  override def toString = "<" + i + ">"
  override lazy val size = 1
}
case class GVar(n: String) extends Term {
  override def toString = n
  override lazy val size = 1
}
case class Abs(t: Term) extends Term {
  override def toString = "(\\" + t + ")"
  override lazy val size = 1 + t.size
}
case class App(t1: Term, t2: Term) extends Term {
  override def toString = "(" + t1 + " " + t2 + ")"
  override lazy val size = t1.size + t2.size
}
// Simple let-expression. `v` is represented by `BVar(0)` in `in`.
case class Let(v: Term, in: Term) extends Term {
  override lazy val size = 1 + v.size + in.size
}
// Fix point combinator.
// Term itself is represented as `BVar(0)` in `t`.
// Really we use only this Fix(Abs(_)) combination.
// TODO: refactor for this certain case
case class Fix(t: Term) extends Term {
  override lazy val size = 1 + t.size
}
case class Ctr(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", ", ")")
  override lazy val size = 1 + args.map(_.size).sum
}
case class Case(sel: Term, branches: List[Branch]) extends Term {
  override def toString = "case " + sel + " of " + branches.map(b => b._1 + " -> " + b._2).mkString("{", "; ", "}")
  override lazy val size = sel.size + branches.map{b => b._2.size}.sum
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

    