package mrsc.counters

import mrsc.core._
import Conf._

// Naive multi-result supercompiler.
class MRCountersRules(protocol: Protocol, l: Int) extends GraphRewriteRules[Conf, Unit] {

  override def steps(g: G): List[S] =
    fold(g) match {
      case None    => rebuild(g) ++ drive(g)
      case Some(s) => List(s)
    }

  def fold(g: G): Option[S] = {
    val c = g.current.conf
    for (n <- g.completeNodes.find(n => instanceOf(c, n.conf)))
      yield FoldStep(n.sPath)
  }

  def drive(g: G): List[S] =
    if (dangerous(g)) List()
    else List(AddChildNodesStep(next(g.current.conf)))

  def rebuild(g: G): List[S] =
    for (c <- gens(g.current.conf))
      yield RebuildStep(c): S

  def dangerous(g: G): Boolean =
    g.current.conf exists { case Num(i) => i >= l; case Omega => false }

  def next(c: Conf): List[(Conf, Unit)] =
    for (Some(c) <- protocol.rules.map(_.lift(c)))
      yield (c, ())
}

// Single-result supercompiler is just
// a specialization of multi-result supercompiler.
class SRCountersRules(protocol: Protocol, l: Int) extends MRCountersRules(protocol, l) {

  def genExpr(e: Expr): Expr =
    if (e >= l) Omega else e

  override def rebuild(g: G): List[S] =
    if (dangerous(g))
      List(RebuildStep(g.current.conf.map(genExpr)))
    else List()
}

// Optimized multi-result supercompiler that takes
// into account some knowledge about domain.
// This is also a specialization.
class FastMRCountersRules(protocol: Protocol, l: Int) extends MRCountersRules(protocol, l) {

  var maxSize: Int = Int.MaxValue

  override def drive(g: G): List[S] =
    for (
      AddChildNodesStep(ns) <- super.drive(g);
      if ns.forall(c => !protocol.unsafe(c._1))
    ) yield AddChildNodesStep(ns)

  override def rebuild(g: G): List[S] =
    for (c <- oneStepGens(g.current.conf) if !protocol.unsafe(c))
      yield RebuildStep(c): S

  override def steps(g: G): List[S] =
    if (protocol.unsafe(g.current.conf) || size(g) > maxSize)
      List()
    else
      super.steps(g)

  private def size(g: G): Int =
    g.completeNodes.size + g.incompleteLeaves.size
}
