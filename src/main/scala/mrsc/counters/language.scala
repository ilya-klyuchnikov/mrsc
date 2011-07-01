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

trait CounterSyntax extends Syntax[Counter] {
  override val instance = CounterInstanceOrdering
  override def subst(c: Counter, sub: Subst[Counter]) = c
  override def rebuildings(c: Counter) = gens(c) map { (_, emptySubst) }
  override def rebuilding2Configuration(rb: Rebuilding[Counter]) = rb._1
  override def findSubst(from: Counter, to: Counter) =
    if (instance.lteq(from, to)) Some(emptySubst) else None
  override def size(c: Counter) = c.size
  def gens(c: Counter): List[Counter] = c match {
    case Nil => List(Nil)
    case e :: c1 => for (cg <- genComp(e); gs <- gens(c1)) yield cg :: gs
  }
  def genComp(c: Component): List[Component] = c match {
    case Omega => List(Omega)
    case value => List(Omega, value)
  }
}

trait MagicWhistle {
  val l: Int
  def isDangerous(counter: Counter) = counter exists {
    case Value(i) => i >= l
    case Omega => false
  }
}

trait MagicGen extends CounterSyntax {
  val l: Int
  override def rebuildings(c: Counter) =
    (c.map(e => if (e >= l) Omega else e), emptySubst) :: Nil
}

trait CounterSemantics extends Semantics[Counter] {
  def rules: List[TransitionRule]
  def applyRules(c: Counter) =
    rules.filter(_.isDefinedAt(c)).map(_(c))
  override def drive(c: Counter) =
    VariantsDriveStep(applyRules(c) map { (emptyContraction, _) })
  override def isDrivable(c: Counter) =
    rules.exists(_.isDefinedAt(c))
}

object ComponentOrdering extends SimplePartialOrdering[Component] {
  def lteq(x: Component, y: Component) = (x, y) match {
    case (Omega, _) => true
    case (_, _) => x == y
  }
}

object CounterInstanceOrdering extends SimplePartialOrdering[Counter] {
  def lteq(c1: Counter, c2: Counter) = (c1, c2).zipped.forall(ComponentOrdering.lteq)
}