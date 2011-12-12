package mrsc.counters

// The language of configurations
sealed trait Component {
  def +(comp: Component): Component
  def -(comp: Component): Component
  def >=(i: Int): Boolean
  def ===(i: Int): Boolean
}

case class Value(i: Int) extends Component {
  override def +(comp: Component) = comp match {
    case Omega    => Omega
    case Value(j) => Value(i + j)
  }
  override def -(comp: Component) = comp match {
    case Omega    => Omega
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

trait Protocol {
  val start: OmegaConf
  val rules: List[TransitionRule]
  def unsafe(c: OmegaConf): Boolean
}

// the syntax of language of configurations
object Configuration {
  def instanceOf(c1: OmegaConf, c2: OmegaConf): Boolean =
    (c1, c2).zipped.forall(instanceOf)

  private def instanceOf(c1: Component, c2: Component) = (c1, c2) match {
    case (_, Omega) => true
    case (_, _)     => c1 == c2
  }

  private def product[T](zzs: List[List[T]]): List[List[T]] = zzs match {
    case Nil =>
      List(List())
    case xs :: xss =>
      for (y <- xs; ys <- product(xss)) yield y :: ys
  }

  def rebuildings(c: OmegaConf) =
    product(c map genComp) - c

  private def genComp(c: Component): List[Component] = c match {
    case Omega => List(Omega)
    case value => List(Omega, value)
  }
}