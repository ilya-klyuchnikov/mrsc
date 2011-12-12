package mrsc.counters

import mrsc.core._

// the very first iteration: multi-result version
// the next step of optimization: if the size of the current graph 
// bigger than the size of the smallest graph, then stop.
case class CountersRules(val protocol: Protocol, l: Int) extends GraphRewriteRules[OmegaConf, Int] {
  type Signal = Boolean
  def inspect(g: G) = g.current.conf exists {
    case Value(i) => i >= l
    case Omega    => false
  }

  def fold(g: G): Option[S] =
    g.completeNodes.find { n => Configuration.instanceOf(g.current.conf, n.conf) } map { n => FoldStep(n.sPath): S }

  def drive(dangerous: Boolean, g: G): List[S] =
    if (dangerous) {
      List()
    } else {
      val subSteps = for ((next, i) <- next(g.current.conf).zipWithIndex if next.isDefined) yield (next.get, i + 1)
      val step: S = if (subSteps.isEmpty) CompleteCurrentNodeStep() else AddChildNodesStep(subSteps)
      List(step)
    }

  def next(c: OmegaConf) = protocol.rules.map { _.lift(c) }

  def rebuild(dangerous: Boolean, g: G): List[S] =
    if (dangerous) {
      List()
    } else {
      Configuration.rebuildings(g.current.conf) map { RebuildStep(_): S }
    }

  override def steps(g: G): List[S] =
    // small optimization
    if (protocol.unsafe(g.current.conf)) {
      List()
    } else {
      fold(g) match {
        case Some(s) => List(s)
        case None =>
          val signal = inspect(g)
          drive(signal, g) ++ rebuild(signal, g)
      }
    }
}