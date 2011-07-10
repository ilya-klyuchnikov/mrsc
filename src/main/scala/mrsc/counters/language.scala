package mrsc.counters

import mrsc._

sealed trait Component {
  def +(comp: Component): Component
  def -(comp: Component): Component
  def >=(i: Int): Boolean
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
  override def >=(j: Int) = i >= j
  override def toString = i.toString
}

case object Omega extends Component {
  def +(comp: Component) = Omega
  def -(comp: Component) = Omega
  def >=(comp: Int) = true
  override def toString = "ϖ"
}

trait CounterPreSyntax extends PreSyntax[Counter] {
  val instance = CounterInstanceOrdering
  def rebuildings(c: Counter) = gens(c) - c
  def gens(c: Counter): List[Counter] = c match {
    case Nil => List(Nil)
    case e :: c1 => for (cg <- genComp(e); gs <- gens(c1)) yield cg :: gs
  }
  def genComp(c: Component): List[Component] = c match {
    case Omega => List(Omega)
    case value => List(Omega, value)
  }
}

trait LWhistle {
  val l: Int
  def isDangerous(counter: Counter) = counter exists {
    case Value(i) => i >= l
    case Omega => false
  }
}

trait CounterRuleSemantics extends RuleSemantics[Counter] {
  val protocol: Protocol
  override def drive(c: Counter) = protocol.rules.map { _.lift(c) }
}

object CounterInstanceOrdering extends SimplePartialOrdering[Counter] {
  def lteq(c1: Counter, c2: Counter) = (c1, c2).zipped.forall(lteq)
  def lteq(x: Component, y: Component) = (x, y) match {
    case (Omega, _) => true
    case (_, _) => x == y
  }
}