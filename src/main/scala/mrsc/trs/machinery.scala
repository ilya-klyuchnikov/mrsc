package mrsc.trs

import mrsc.core._
import mrsc.pfp.{Extra, NoExtra}

trait GenericMultiMachine[C, D, E] extends Machine[C, D, E] {

  type W = Option[CoNode[C, D, E]]
  def isLeaf(coGraph: CG): Boolean
  def fold(coGraph: CG): Option[CoPath]
  def blame(coGraph: CG): W
  def drive(whistle: W, coGraph: CG): List[CMD]
  def rebuildings(whistle: W, coGraph: CG): List[CMD]

  /*! The logic of this machine is straightforward:
     
     * If there are opportunities for folding, lets fold.
     
     * Otherwise, lets try all variants.
    
   Note that the whistle signal is passed to `drive`, `rebuildings` and `tricks`.
  */
  override def steps(coGraph: CG): List[CMD] =
    if (isLeaf(coGraph))
      List(ConvertToLeaf)
    else fold(coGraph) match {
      case Some(path) =>
        List(Fold(path))
      case _ =>
        val signal = blame(coGraph)
        val driveSteps = drive(signal, coGraph)
        val rebuildSteps = rebuildings(signal, coGraph)
        driveSteps ++ rebuildSteps
    }
}

trait SimpleInstanceFolding[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] {
  override def fold(coGraph: PartialCoGraph[C, D, Extra[C]]): Option[CoPath] =
    coGraph.current.ancestors.find { n => instance.lteq(n.conf, coGraph.current.conf) } map { _.coPath }
}

trait SimpleInstanceFoldingToAny[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] {
  override def fold(coGraph: PartialCoGraph[C, D, Extra[C]]): Option[CoPath] =
    coGraph.complete.find { n => instance.lteq(n.conf, coGraph.current.conf) } map { _.coPath }
}

trait SimpleUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] {
  def unsafe(c: C): Boolean
  override def blame(coGraph: PartialCoGraph[C, D, Extra[C]]): W =
    if (unsafe(coGraph.current.conf)) Some(coGraph.current) else None
}

trait SimpleCurrentGensOnWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: W, coGraph: PartialCoGraph[C, D, Extra[C]]): List[Command[C, D, Extra[C]]] = {
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val rbs = rebuildings(coGraph.current.conf) filterNot unsafe
        rbs map { Rebuild(_, NoExtra) }
    }
  }
}

trait SimpleGensWithUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: W, coGraph: PartialCoGraph[C, D, Extra[C]]): List[Command[C, D, Extra[C]]] = {
    val rbs = rebuildings(coGraph.current.conf) filterNot unsafe
    rbs map { Rebuild(_, NoExtra) }
  }
}

trait RuleDriving[C] extends GenericMultiMachine[C, Int, Extra[C]] with RewriteSemantics[C] {
  override def drive(whistle: W, coGraph: PartialCoGraph[C, Int, Extra[C]]): List[Command[C, Int, Extra[C]]] =
    whistle match {
      case Some(blamed) => List(Discard)
      case None =>
        val subSteps =
          for ((next, i) <- drive(coGraph.current.conf).zipWithIndex if next.isDefined)
            yield (next.get, i + 1, NoExtra)
        List(AddChildNodes(subSteps))
    }

  override def isLeaf(coGraph: PartialCoGraph[C, Int, Extra[C]]) =
    false
}