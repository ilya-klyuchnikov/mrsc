package mrsc.trs

import mrsc.core._
import mrsc.pfp.{Extra, NoExtra}

trait GenericMultiMachine[C, D, E] extends Machine[C, D, E] {

  type W = Option[CoNode[C, D, E]]
  def isLeaf(g: CG): Boolean
  def fold(g: CG): Option[CoPath]
  def blame(g: CG): W
  def drive(whistle: W, g: CG): List[CG]
  def rebuildings(whistle: W, g: CG): List[CG]

  /*! The logic of this machine is straightforward:
     
     * If there are opportunities for folding, lets fold.
     
     * Otherwise, lets try all variants.
    
   Note that the whistle signal is passed to `drive`, `rebuildings` and `tricks`.
  */
  override def steps(g: CG): List[CG] =
    if (isLeaf(g))
      List(g.convertToLeaf())
    else fold(g) match {
      case Some(path) =>
        List(g.fold(path))
      case _ =>
        val signal = blame(g)
        val driveSteps = drive(signal, g)
        val rebuildSteps = rebuildings(signal, g)
        driveSteps ++ rebuildSteps
    }
}

trait SimpleInstanceFolding[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] {
  override def fold(g: PartialCoGraph[C, D, Extra[C]]): Option[CoPath] =
    g.current.ancestors.find { n => instance.lteq(n.conf, g.current.conf) } map { _.coPath }
}

trait SimpleInstanceFoldingToAny[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] {
  override def fold(g: PartialCoGraph[C, D, Extra[C]]): Option[CoPath] =
    g.completeNodes.find { n => instance.lteq(n.conf, g.current.conf) } map { _.coPath }
}

trait SimpleUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] {
  def unsafe(c: C): Boolean
  override def blame(g: PartialCoGraph[C, D, Extra[C]]): W =
    if (unsafe(g.current.conf)) Some(g.current) else None
}

trait SimpleCurrentGensOnWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: W, g: PartialCoGraph[C, D, Extra[C]]): List[PartialCoGraph[C, D, Extra[C]]] = {
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val rbs = rebuildings(g.current.conf) filterNot unsafe
        rbs map { g.rebuild(_, NoExtra) }
    }
  }
}

trait SimpleGensWithUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: W, g: PartialCoGraph[C, D, Extra[C]]): List[PartialCoGraph[C, D, Extra[C]]] = {
    val rbs = rebuildings(g.current.conf) filterNot unsafe
    rbs map { g.rebuild(_, NoExtra) }
  }
}

trait RuleDriving[C] extends GenericMultiMachine[C, Int, Extra[C]] with RewriteSemantics[C] {
  override def drive(whistle: W, g: PartialCoGraph[C, Int, Extra[C]]): List[PartialCoGraph[C, Int, Extra[C]]] =
    whistle match {
      case Some(blamed) =>
        List(g.toUnworkable())
      case None =>
        val subSteps =
          for ((next, i) <- drive(g.current.conf).zipWithIndex if next.isDefined)
            yield (next.get, i + 1, NoExtra)
        List(g.addChildNodes(subSteps))
    }

  override def isLeaf(g: PartialCoGraph[C, Int, Extra[C]]) =
    false
}