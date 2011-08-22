package mrsc.trs

trait TRSSyntax[C] {
  def rebuildings(c: C): List[C]
  def equiv(c1: C, c2: C): Boolean 
  def instanceOf(c1: C, c2: C): Boolean
}

// There are n rules. Variants should return a list of length n
trait RewriteSemantics[C] {
  def drive(c: C): List[Option[C]]
}