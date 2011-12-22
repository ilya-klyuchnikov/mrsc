package mrsc.counters

// The language of configurations
sealed trait Expr {
  def +(comp: Expr): Expr
  def -(comp: Expr): Expr
  def >=(i: Int): Boolean
  def ===(i: Int): Boolean
}

case class Num(i: Int) extends Expr {
  override def +(comp: Expr) = comp match {
    case Omega    => Omega
    case Num(j) => Num(i + j)
  }
  override def -(comp: Expr) = comp match {
    case Omega    => Omega
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

trait Protocol {
  val start: Conf
  val rules: List[TransitionRule]
  def unsafe(c: Conf): Boolean
}

// the syntax of language of configurations
object Configuration {
  def instanceOf(c1: Conf, c2: Conf): Boolean =
    (c1, c2).zipped.forall(instanceOf)

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

  def rebuildings(c: Conf): List[Conf] =
    (0 until c.size).toList.collect { i => c(i) match { case Num(j) if j >= 0 => c.updated(i, Omega) } }

  private def genComp(c: Expr): List[Expr] = c match {
    case Omega              => List(Omega)
    // TODO: -1, -2, ...
    case Num(i) if i >= 0 => List(Omega, Num(i))
    case v                  => List(v)
  }
}