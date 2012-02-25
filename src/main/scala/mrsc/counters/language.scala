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
  def +(comp: Expr) = comp match {
    case Omega  => Omega
    case Num(j) => Num(i + j)
  }
  def -(comp: Expr) = comp match {
    case Omega  => Omega
    case Num(j) => Num(i - j)
  }
  def ===(j: Int) = i == j
  def >=(j: Int) = i >= j
  override def toString = i.toString
}

case object Omega extends Expr {
  def +(comp: Expr) = Omega
  def -(comp: Expr) = Omega
  def >=(comp: Int) = true
  def ===(j: Int) = true
  override def toString = "ω"
}

// The "syntax" of language of configurations.
// Ad-hoc: we do not rebuild negative nums: negative nums (-1, -2, -3, -4) are used in
// the encoding of Java protocol for encoding of 4 commands.
object Conf {
  def instanceOf(c1: Conf, c2: Conf): Boolean =
    (c1, c2).zipped.forall((e1, e2) => e1 == e2 || e2 == Omega)

  def gens(c: Conf) =
    product(c map genExpr) - c

  def oneStepGens(c: Conf): List[Conf] =
    for (i <- List.range(0, c.size) if c(i) != Omega && c(i) >= 0)
      yield c.updated(i, Omega)

  def product[T](zs: List[List[T]]): List[List[T]] = zs match {
    case Nil       => List(List())
    case x :: xs => for (y <- x; ys <- product(xs)) yield y :: ys
  }

  private def genExpr(c: Expr): List[Expr] = c match {
    case Omega            => List(Omega)
    case Num(i) if i >= 0 => List(Omega, Num(i))
    case v                => List(v)
  }
}