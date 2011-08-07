package mrsc.counters

import mrsc.core._
import mrsc.pfp._

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

trait CountersPreSyntax extends PreSyntax[OmegaConf] {
  val instance = OmegaConfInstanceOrdering
  def rebuildings(c: OmegaConf) = gens(c) - c
  def gens(c: OmegaConf): List[OmegaConf] = c match {
    case Nil => List(Nil)
    case e :: c1 => for (cg <- genComp(e); gs <- gens(c1)) yield cg :: gs
  }
  def genComp(c: Component): List[Component] = c match {
    case Omega => List(Omega)
    case value => List(Omega, value)
  }
}

trait CountersSemantics extends RewriteSemantics[OmegaConf] {
  val protocol: Protocol
  def drive(c: OmegaConf) = protocol.rules.map { _.lift(c) }
}

trait LWhistle {
  val l: Int
  def unsafe(counter: OmegaConf) = counter exists {
    case Value(i) => i >= l
    case Omega => false
  }
}

object OmegaConfInstanceOrdering extends SimplePartialOrdering[OmegaConf] {
  def lteq(c1: OmegaConf, c2: OmegaConf) = (c1, c2).zipped.forall(lteq)
  def lteq(x: Component, y: Component) = (x, y) match {
    case (Omega, _) => true
    case (_, _) => x == y
  }
}