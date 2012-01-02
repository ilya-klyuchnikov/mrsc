package mrsc.counters

import mrsc.core._

// There are some optimizations in this code in order to filter out failures
// and reduce the number of alternatives.
case class MRCountersRules(val protocol: Protocol, l: Int) extends GraphRewriteRules[Conf, Int] {
  var maxSize: Int = Int.MaxValue
  def inspect(g: G) = g.current.conf exists {
    case Num(i) => i >= l
    case Omega  => false
  }

  def fold(g: G): Option[S] =
    g.completeNodes.find { n => Conf.instanceOf(g.current.conf, n.conf) } map { n => FoldStep(n.sPath): S }

  def drive(dangerous: Boolean, g: G): List[S] =
    if (dangerous) {
      List()
    } else {
      val subSteps = for ((next, i) <- next(g.current.conf).zipWithIndex if next.isDefined) yield (next.get, i + 1)
      if (subSteps.isEmpty) {
        List(CompleteCurrentNodeStep())
      } else {
        // SIC: optimization
        if (subSteps.exists { case (n, i) => protocol.unsafe(n) }) {
          List()
        } else {
          List(AddChildNodesStep(subSteps))
        }
      }
    }

  def next(c: Conf) = protocol.rules.map { _.lift(c) }

  def rebuild(dangerous: Boolean, g: G): List[S] =
    Conf.oneStepRebuildings(g.current.conf) filter { !protocol.unsafe(_) } map { RebuildStep(_): S }

  override def steps(g: G): List[S] =
    // SIC: optimization
    if (protocol.unsafe(g.current.conf) || size(g) > maxSize) {
      List()
    } else {
      fold(g) match {
        case Some(s) => List(s)
        case None =>
          val signal = inspect(g)
           rebuild(signal, g) ++ drive(signal, g)
      }
    }
  
  private def size(g: G) =
    g.completeNodes.size + g.incompleteLeaves.size
}

// A (not elegant so far) version of single-result supercompiler for counters.
case class SRCountersRules(val protocol: Protocol, l: Int) extends GraphRewriteRules[Conf, Int] {
  def inspect(g: G) = g.current.conf exists {
    case Num(i) => i >= l
    case Omega  => false
  }

  def fold(g: G): Option[S] =
    g.completeNodes.find { n => Conf.instanceOf(g.current.conf, n.conf) } map { n => FoldStep(n.sPath): S }

  def drive(dangerous: Boolean, g: G): List[S] =
    if (dangerous) {
      List()
    } else {
      val subSteps = for ((next, i) <- next(g.current.conf).zipWithIndex if next.isDefined) yield (next.get, i + 1)
      if (subSteps.isEmpty) {
        List(CompleteCurrentNodeStep())
      } else {
        List(AddChildNodesStep(subSteps))
      }
    }

  def next(c: Conf) = protocol.rules.map { _.lift(c) }

  def rebuild(dangerous: Boolean, g: G): List[S] =
    if (dangerous) {
      List(RebuildStep(g.current.conf.map { e => if (e >= l) Omega else e }))
    } else {
      List()
    }

  override def steps(g: G): List[S] =
    fold(g) match {
      case Some(s) => List(s)
      case None =>
        val signal = inspect(g)
        drive(signal, g) ++ rebuild(signal, g)
    }
}