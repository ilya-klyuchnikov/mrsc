package mrsc.counters

import mrsc.core._

// No optimizations at all
case class MRCountersRules1(val protocol: Protocol, l: Int) extends GraphRewriteRules[Conf, Int] {
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
        List(AddChildNodesStep(subSteps))
      }
    }

  def next(c: Conf) = protocol.rules.map { _.lift(c) }

  def rebuild(dangerous: Boolean, g: G): List[S] =
    Conf.rebuildings(g.current.conf) map { RebuildStep(_): S }

  override def steps(g: G): List[S] =
    fold(g) match {
      case Some(s) => List(s)
      case None =>
        val signal = inspect(g)
        rebuild(signal, g) ++ drive(signal, g)
    }

  private def size(g: G) =
    g.completeNodes.size + g.incompleteLeaves.size
}

// The copy of MRCountersRules0 - with the only change:
// instead of considering all rebuildings, we consider only one-step 
// rebuildings here
case class MRCountersRules2(val protocol: Protocol, l: Int) extends GraphRewriteRules[Conf, Int] {
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
        List(AddChildNodesStep(subSteps))
      }
    }

  def next(c: Conf) = protocol.rules.map { _.lift(c) }

  def rebuild(dangerous: Boolean, g: G): List[S] =
    // THE CHANGE 1:
    Conf.oneStepRebuildings(g.current.conf) map { RebuildStep(_): S }

  override def steps(g: G): List[S] =
    fold(g) match {
      case Some(s) => List(s)
      case None =>
        val signal = inspect(g)
        rebuild(signal, g) ++ drive(signal, g)
    }

  private def size(g: G) =
    g.completeNodes.size + g.incompleteLeaves.size
}

// The copy of MRCountersRules1 - we consider only
// safe rebuildings
case class MRCountersRules3(val protocol: Protocol, l: Int) extends GraphRewriteRules[Conf, Int] {
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
        List(AddChildNodesStep(subSteps))
      }
    }

  def next(c: Conf) = protocol.rules.map { _.lift(c) }

  def rebuild(dangerous: Boolean, g: G): List[S] =
    // CHANGE1 + CHANGE2:
    Conf.oneStepRebuildings(g.current.conf) filter { !protocol.unsafe(_) } map { RebuildStep(_): S }

  override def steps(g: G): List[S] =
    fold(g) match {
      case Some(s) => List(s)
      case None =>
        val signal = inspect(g)
        rebuild(signal, g) ++ drive(signal, g)
    }

  private def size(g: G) =
    g.completeNodes.size + g.incompleteLeaves.size
}

// The copy of MRCountersRules2 
// The next change (#3): 
// we ban driving step if one of the next configurations
// is unsafe
case class MRCountersRules4(val protocol: Protocol, l: Int) extends GraphRewriteRules[Conf, Int] {
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
        // CHANGE #3
        if (subSteps.exists { case (n, i) => protocol.unsafe(n) }) {
          List()
        } else {
          List(AddChildNodesStep(subSteps))
        }
      }
    }

  def next(c: Conf) = protocol.rules.map { _.lift(c) }

  def rebuild(dangerous: Boolean, g: G): List[S] =
    // CHANGE1 + CHANGE2:
    Conf.oneStepRebuildings(g.current.conf) filter { !protocol.unsafe(_) } map { RebuildStep(_): S }

  override def steps(g: G): List[S] =
    fold(g) match {
      case Some(s) => List(s)
      case None =>
        val signal = inspect(g)
        rebuild(signal, g) ++ drive(signal, g)
    }

  private def size(g: G) =
    g.completeNodes.size + g.incompleteLeaves.size
}

// The most optimized solution
// The final change: we have the size of minimal graph so far
case class MRCountersRules5(val protocol: Protocol, l: Int) extends GraphRewriteRules[Conf, Int] {
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
    // SIC: one more optimization here
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