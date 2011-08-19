package mrsc.trs

import mrsc.core._
import mrsc.pfp.{Extra, NoExtra}

trait GenericMultiMachine[C, D, E] extends Machine[C, D, E] {

  type WS = Node[C, D, E]
  def unsafe(g: G): Boolean = false
  def canFold(g: G): Option[Path]
  def inspect(g: G): Option[WS]
  def drive(whistle: Option[WS], g: G): List[G]
  def rebuildings(whistle: Option[WS], g: G): List[G]

  /*! The logic of this machine is straightforward:
     
     * If there are opportunities for folding, lets fold.
     
     * Otherwise, lets try all variants.
    
   Note that the whistle signal is passed to `drive`, `rebuildings` and `tricks`.
  */
  override def steps(g: G): List[G] =
    if (unsafe(g))
      List(g.toUnworkable())
    else canFold(g) match {
      case Some(path) =>
        List(g.fold(path))
      case None =>
        val whistle = inspect(g)
        val driveSteps = drive(whistle, g)
        val rebuildSteps = rebuildings(whistle, g)
        driveSteps ++ rebuildSteps
    }
}

trait SafetyAware[C, D] extends GenericMultiMachine[C, D, Extra[C]] {
  def unsafe(c: C): Boolean
  override def unsafe(g: G): Boolean = unsafe(g.current.conf)
}

trait SimpleInstanceFolding[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] {
  override def canFold(g: Graph[C, D, Extra[C]]): Option[Path] =
    g.current.ancestors.find { n => instanceOf(g.current.conf, n.conf) } map { _.path }
}

trait SimpleInstanceFoldingToAny[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] {
  override def canFold(g: Graph[C, D, Extra[C]]): Option[Path] =
    g.completeNodes.find { n => instanceOf(g.current.conf, n.conf) } map { _.path }
}

trait SimpleUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] {
  def dubious(c: C): Boolean
  override def inspect(g: Graph[C, D, Extra[C]]): Option[WS] =
    if (dubious(g.current.conf)) Some(g.current) else None
}

trait SimpleCurrentGensOnWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: Option[WS], g: Graph[C, D, Extra[C]]): List[Graph[C, D, Extra[C]]] = {
    whistle match {
      case None =>
        List()
      case Some(_) =>
        val rbs = rebuildings(g.current.conf) filterNot dubious
        rbs map { g.rebuild(_, NoExtra) }
    }
  }
}

trait SimpleGensWithUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: Option[WS], g: Graph[C, D, Extra[C]]): List[Graph[C, D, Extra[C]]] = {
    val rbs = rebuildings(g.current.conf) filterNot dubious
    rbs map { g.rebuild(_, NoExtra) }
  }
}

trait RuleDriving[C] extends GenericMultiMachine[C, Int, Extra[C]] with RewriteSemantics[C] {
  override def drive(whistle: Option[WS], g: Graph[C, Int, Extra[C]]): List[Graph[C, Int, Extra[C]]] =
    whistle match {
      case Some(_) =>
        List(g.toUnworkable())
      case None =>
        val subSteps =
          for ((next, i) <- drive(g.current.conf).zipWithIndex if next.isDefined)
            yield (next.get, i + 1, NoExtra)
        List(g.addChildNodes(subSteps))
    }
}