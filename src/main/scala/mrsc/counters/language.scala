package mrsc.counters

// The main entity
trait Protocol {
  val start: Conf
  val rules: List[TransitionRule]
  def unsafe(c: Conf): Boolean
  def isabelleEncoding: String
  def name: String
}

// The language of configurations
// The type of configurations is defined in package.scala
// as type Conf = List[Expr]

// part of configuration:
// Expr is either Num(i) or Omega
sealed trait Expr {
  def +(comp: Expr): Expr
  def -(comp: Expr): Expr
  def >=(i: Int): Boolean
  def ===(i: Int): Boolean
}

case class Num(i: Int) extends Expr {
  override def +(comp: Expr) = comp match {
    case Omega  => Omega
    case Num(j) => Num(i + j)
  }
  override def -(comp: Expr) = comp match {
    case Omega  => Omega
    case Num(j) => Num(i - j)
  }
  override def ===(j: Int) = i == j
  override def >=(j: Int) = i >= j
  override def toString = i.toString
}

case object Omega extends Expr {
  def +(comp: Expr) = Omega
  def -(comp: Expr) = Omega
  def >=(comp: Int) = true
  override def ===(j: Int) = true
  override def toString = "ϖ"
}

// The "syntax" of language of configurations.
// Ad-hoc: we do not rebuild negative nums: negative nums (-1, -2, -3, -4) are used in
// the encoding of Java protocol for encoding of 4 commands.
object Conf {
  def instanceOf(c1: Conf, c2: Conf): Boolean = (c1, c2).zipped.forall(instanceOf)

  def rebuildings(c: Conf) =
    product(c map rebuildComp) - c

  def oneStepRebuildings(c: Conf): List[Conf] =
    (0 until c.size).toList.collect { i => c(i) match { case Num(j) if j >= 0 => c.updated(i, Omega) } }

  private def instanceOf(c1: Expr, c2: Expr) = (c1, c2) match {
    case (_, Omega) => true
    case (_, _)     => c1 == c2
  }

  private def product[T](zzs: List[List[T]]): List[List[T]] = zzs match {
    case Nil =>
      List(List())
    case xs :: xss =>
      for (y <- xs; ys <- product(xss)) yield y :: ys
  }

  private def rebuildComp(c: Expr): List[Expr] = c match {
    case Omega            => List(Omega)
    case Num(i) if i >= 0 => List(Omega, Num(i))
    case v                => List(v)
  }
}