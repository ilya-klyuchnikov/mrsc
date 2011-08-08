package mrsc.trs

trait PreSyntax[C] {
  def instance: PartialOrdering[C]
  def rebuildings(c: C): List[C]
}

// There are n rules. Variants should return a list of length n
trait RewriteSemantics[C] {
  def drive(c: C): List[Option[C]]
}