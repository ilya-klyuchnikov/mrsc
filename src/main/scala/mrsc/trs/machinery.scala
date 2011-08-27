package mrsc.trs

import mrsc.core._

trait GenericMultiMachine[C, D, E]
  extends Machine2[C, D, E]
  with MachineSteps[C, D, E] {

  type Warning = Node[C, D, E]
  def unsafe(g: G): Boolean = false
  def canFold(g: G): Option[Node[C, D, E]]
  def mayDiverge(g: G): Option[Warning]
  def drive(whistle: Option[Warning], g: G): List[S]
  def rebuildings(whistle: Option[Warning], g: G): List[S]

  /*! The logic of this machine is straightforward:
     
     * If there are opportunities for folding, lets fold.
     
     * Otherwise, lets try all variants.
    
   Note that the whistle signal is passed to `drive`, `rebuildings` and `tricks`.
  */
  override def steps(g: G): List[S] =
    if (unsafe(g))
      List(toUnworkable)
    else canFold(g) match {
      case Some(node) =>
        List(fold(node))
      case None =>
        val whistle = mayDiverge(g)
        val driveSteps = drive(whistle, g)
        val rebuildSteps = rebuildings(whistle, g)
        driveSteps ++ rebuildSteps
    }
}

trait SafetyAware[C, D] extends GenericMultiMachine[C, D, Unit] {
  def unsafe(c: C): Boolean
  override def unsafe(g: G): Boolean = {
    assert(!g.isComplete)
    unsafe(g.current.conf)
  }
}

trait SimpleInstanceFolding[C, D] extends GenericMultiMachine[C, D, Unit] with TRSSyntax[C] {
  override def canFold(g: Graph[C, D, Unit]): Option[Node[C, D, Unit]] =
    g.current.ancestors.find { n => instanceOf(g.current.conf, n.conf) }
}

trait SimpleInstanceFoldingToAny[C, D] extends GenericMultiMachine[C, D, Unit] with TRSSyntax[C] {
  override def canFold(g: Graph[C, D, Unit]): Option[Node[C, D, Unit]] =
    g.completeNodes.find { n => instanceOf(g.current.conf, n.conf) }
}

trait SimpleUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Unit] {
  def dangerous(c: C): Boolean
  override def mayDiverge(g: Graph[C, D, Unit]): Option[Warning] =
    if (dangerous(g.current.conf)) Some(g.current) else None
}

trait SimpleCurrentGensOnWhistle[C, D] extends GenericMultiMachine[C, D, Unit] with TRSSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: Option[Warning], g: Graph[C, D, Unit]): List[S] = {
    whistle match {
      case None =>
        List()
      case Some(_) =>
        val rbs = rebuildings(g.current.conf) filterNot dangerous
        rbs map { rebuild(_, ()) }
    }
  }
}

trait SimpleGensWithUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Unit] with TRSSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: Option[Warning], g: Graph[C, D, Unit]): List[S] = {
    val rbs = rebuildings(g.current.conf) filterNot dangerous
    rbs map { rebuild(_, ()) }
  }
}

trait RuleDriving[C] extends GenericMultiMachine[C, Int, Unit] with RewriteSemantics[C] {
  override def drive(whistle: Option[Warning], g: Graph[C, Int, Unit]): List[S] =
    whistle match {
      case Some(_) =>
        List(toUnworkable)
      case None =>
        val subSteps =
          for ((next, i) <- drive(g.current.conf).zipWithIndex if next.isDefined)
            yield (next.get, i + 1, ())
        if (subSteps.isEmpty)
          List(completeCurrentNode)
        else
          List(addChildNodes(subSteps))
    }
}