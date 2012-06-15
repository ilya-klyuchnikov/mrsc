package mrsc.pfp

abstract sealed trait Step
case object TransientStep extends Step {
  override def toString = "->"
}
case class CaseBranch(term: Term, ptr: Ptr, alt: Ctr) extends Step {
  override def toString = term + " = " + alt
}
case object CtrArg extends Step {
  override def toString = ""
}
case object GenSkel extends Step {
  override def toString = "let"
}
case class GenPart(t: Term) extends Step {
  override def toString = t + "->"
}
