package mrsc.trs.counters

import mrsc.trs._

sealed trait Component {
  def +(comp: Component): Component
  def -(comp: Component): Component
  def >=(i: Int): Boolean
  def ===(i: Int): Boolean
}

case class Value(i: Int) extends Component {
  override def +(comp: Component) = comp match {
    case Omega => Omega
    case Value(j) => Value(i + j)
  }
  override def -(comp: Component) = comp match {
    case Omega => Omega
    case Value(j) => Value(i - j)
  }
  override def ===(j: Int) = i == j
  override def >=(j: Int) = i >= j
  override def toString = i.toString
}

case object Omega extends Component {
  def +(comp: Component) = Omega
  def -(comp: Component) = Omega
  def >=(comp: Int) = true
  override def ===(j: Int) = true
  override def toString = "ϖ"
}

trait CountersSyntax extends TRSSyntax[OmegaConf] {
  def equiv(c1: OmegaConf, c2: OmegaConf) = CountersSyntax.equiv(c1, c2)
  def instanceOf(c1: OmegaConf, c2: OmegaConf) = CountersSyntax.instanceOf(c1, c2)
  def rebuildings(c: OmegaConf) = CountersSyntax.rebuildings(c)
}

object CountersSyntax {
  def equiv(c1: OmegaConf, c2: OmegaConf) = c1 == c2

  def instanceOf(c1: OmegaConf, c2: OmegaConf): Boolean =
    (c1, c2).zipped.forall(instanceOf)

  def instanceOf(x: Component, y: Component) = (x, y) match {
    case (_, Omega) => true
    case (_, _) => x == y
  }

  private def cartProd[T](zzs: List[List[T]]): List[List[T]] = zzs match {
    case Nil => List(List())
    case xs :: xss => for (y <- xs; ys <- cartProd(xss)) yield y :: ys
  }

  def rebuildings(c: OmegaConf) = cartProd(c map genComp) - c

  private def genComp(c: Component): List[Component] = c match {
    case Omega => List(Omega)
    case value => List(Omega, value)
  }
}

trait CountersSemantics extends RewriteSemantics[OmegaConf] {
  val protocol: Protocol
  def driveConf(c: OmegaConf) = protocol.rules.map { _.lift(c) }
}

trait LWhistle {
  val l: Int
  def dangerous(counter: OmegaConf) = counter exists {
    case Value(i) => i >= l
    case Omega => false
  }
}
