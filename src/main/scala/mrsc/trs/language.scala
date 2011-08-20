package mrsc.trs

import mrsc.core._

trait TRSSyntax[C] extends EquivAndInstanceOf[C] {
  def rebuildings(c: C): List[C]
}

// There are n rules. Variants should return a list of length n
trait RewriteSemantics[C] {
  def drive(c: C): List[Option[C]]
}