package mrsc.counters

import mrsc._

sealed trait Component {
  def +(comp: Component): Component
  def -(comp: Component): Component
  def >=(i: Int): Boolean
}
case class Value(i: Int) extends Component {
  def +(comp: Component): Component = comp match {
    case Omega => Omega
    case Value(j) => Value(i + j)
  }
  def -(comp: Component): Component = comp match {
    case Omega => Omega
    case Value(j) => Value(i - j)
  }
  def >=(j: Int): Boolean = i >= j
  override def toString = i.toString
}
case object Omega extends Component {
  def +(comp: Component): Component = Omega
  def -(comp: Component): Component = Omega
  def >=(comp: Int): Boolean = true
  override def toString = "ϖ"
}

trait CounterSyntax extends Syntax[Counter] {
  val instance: PartialOrdering[Counter] = CounterInstanceOrdering
  override def subst(c: Counter, sub: Subst[Counter]): Counter = c
  def rebuildings(c: Counter): List[Rebuilding[Counter]] =
    counterGens(c) map { (_, Map.empty[Name, Counter]) }
  def counterGens(c: Counter): List[Counter] = c match {
    case Nil =>
      List((Nil))
    case comp :: comps =>
      for (compGen <- gens(comp); rebs <- counterGens(comps)) yield compGen :: rebs
  }
  override def rebuilding2Configuration(rebuilding: Rebuilding[Counter]): Counter = rebuilding._1
  override def findSubst(from: Counter, to: Counter): Option[Subst[Counter]] =
    if (instance.lteq(from, to)) Some(Map()) else None
  override def size(c: Counter): Int = c.size
  def gens(c: Component): List[Component] = c match {
    case Omega => List(Omega)
    case value => List(value, Omega)
  }
}

trait CounterMagic extends CounterSyntax {
  def l: Int
  override def rebuildings(c: Counter): List[Rebuilding[Counter]] = {
    val c1 = c map {
      case Value(i) if i >= l => Omega
      case comp => comp
    }
    List((c1, Map.empty[Name, Counter]))
  }

  def isDangerous(counter: Counter) = counter exists {
    case Value(i) => i >= l
    case Omega => false
  }
}

// Applicative Functor should be use here!
trait CounterSemantics extends Semantics[Counter] {
  def rules: List[TransitionRule]
  def drive(c: Counter): DriveStep[Counter] =
    VariantsDriveStep(applyRules(c) map { (Contraction(null, null), _) })
  def applyRules(c: Counter) =
    for (rule <- rules if rule.isDefinedAt(c)) yield rule(c)
  def isDrivable(c: Counter): Boolean = !applyRules(c).isEmpty
}

object ComponentOrdering extends SimplePartialOrdering[Component] {
  def lteq(x: Component, y: Component): Boolean = (x, y) match {
    case (Omega, _) => true
    case (Value(i), Value(j)) => i == j
    case (_, _) => false
  }
}

// Whistle combinators!!
object CounterInstanceOrdering extends SimplePartialOrdering[Counter] {
  def lteq(x: Counter, y: Counter) = (x, y).zipped.forall(ComponentOrdering.lteq)
}