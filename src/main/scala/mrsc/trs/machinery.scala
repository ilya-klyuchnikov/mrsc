package mrsc.trs

import mrsc.core._
import mrsc.pfp.{Extra, NoExtra}

trait GenericMultiMachine[C, D, E] extends Machine[C, D, E] {

  type W = Option[CoNode[C, D, E]]
  def isLeaf(pState: PS): Boolean
  def fold(pState: PS): Option[CoPath]
  def blame(pState: PS): W
  def drive(whistle: W, pState: PS): List[CMD]
  def rebuildings(whistle: W, pState: PS): List[CMD]

  /*! The logic of this machine is straightforward:
     
     * If there are opportunities for folding, lets fold.
     
     * Otherwise, lets try all variants.
    
   Note that the whistle signal is passed to `drive`, `rebuildings` and `tricks`.
  */
  override def steps(pState: PS): List[CMD] =
    if (isLeaf(pState))
      List(ConvertToLeaf)
    else fold(pState) match {
      case Some(path) =>
        List(Fold(path))
      case _ =>
        val signal = blame(pState)
        val driveSteps = drive(signal, pState)
        val rebuildSteps = rebuildings(signal, pState)
        driveSteps ++ rebuildSteps
    }
}

trait SimpleInstanceFolding[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] {
  override def fold(pState: PState[C, D, Extra[C]]): Option[CoPath] =
    pState.current.ancestors.find { n => instance.lteq(n.conf, pState.current.conf) } map { _.coPath }
}

trait SimpleInstanceFoldingToAny[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] {
  override def fold(pState: PState[C, D, Extra[C]]): Option[CoPath] =
    pState.complete.find { n => instance.lteq(n.conf, pState.current.conf) } map { _.coPath }
}

trait SimpleUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] {
  def unsafe(c: C): Boolean
  override def blame(pState: PState[C, D, Extra[C]]): W =
    if (unsafe(pState.current.conf)) Some(pState.current) else None
}

trait SimpleCurrentGensOnWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: W, pState: PState[C, D, Extra[C]]): List[Command[C, D, Extra[C]]] = {
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val rbs = rebuildings(pState.current.conf) filterNot unsafe
        rbs map { Rebuild(_, NoExtra) }
    }
  }
}

trait SimpleGensWithUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Extra[C]] with PreSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: W, pState: PState[C, D, Extra[C]]): List[Command[C, D, Extra[C]]] = {
    val rbs = rebuildings(pState.current.conf) filterNot unsafe
    rbs map { Rebuild(_, NoExtra) }
  }
}

trait RuleDriving[C] extends GenericMultiMachine[C, Int, Extra[C]] with RewriteSemantics[C] {
  override def drive(whistle: W, pState: PState[C, Int, Extra[C]]): List[Command[C, Int, Extra[C]]] =
    whistle match {
      case Some(blamed) => List(Discard)
      case None =>
        val subSteps =
          for ((next, i) <- drive(pState.current.conf).zipWithIndex if next.isDefined)
            yield (next.get, i + 1, NoExtra)
        List(AddChildNodes(subSteps))
    }

  override def isLeaf(pState: PState[C, Int, Extra[C]]) =
    false
}