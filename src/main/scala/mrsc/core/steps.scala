package mrsc.core

/*!# Abstract steps
   Under the hood an abstract machine deals with some kind of semantics of the language.
   Low-level operations should be translated into high-level abstract operations (or messages) 
   over SC graphs.
*/
sealed trait GraphStep[C, D] extends (SGraph[C, D] => SGraph[C, D])

case class CompleteCurrentNodeStep[C, D] extends GraphStep[C, D] {
  def apply(g: SGraph[C, D]) =
    SGraph(g.incompleteLeaves.tail, g.current :: g.completeLeaves, g.current :: g.completeNodes)
}

case class AddChildNodesStep[C, D](ns: List[(C, D)]) extends GraphStep[C, D] {
  def apply(g: SGraph[C, D]) = {
    val deltaLeaves: List[SNode[C, D]] = ns.zipWithIndex map {
      case ((conf, dInfo), i) =>
        val in = SEdge(g.current, dInfo)
        SNode(conf, in, None, i :: g.current.sPath)
    }
    // Now it is depth-first traversal. If you change 
    // deltaLeaves ++ ls -> ls ++ deltaLeaves,
    // you will have breadth-first traversal
    SGraph(deltaLeaves ++ g.incompleteLeaves.tail, g.completeLeaves, g.current :: g.completeNodes)
  }
}

case class FoldStep[C, D](baseNode: SNode[C, D]) extends GraphStep[C, D] {
  def apply(g: SGraph[C, D]) = {
    val node = g.current.copy(base = Some(baseNode.sPath))
    SGraph(g.incompleteLeaves.tail, node :: g.completeLeaves, node :: g.completeNodes)
  }
}

case class RebuildStep[C, D](c: C) extends GraphStep[C, D] {
  def apply(g: SGraph[C, D]) = {
    val node = g.current.copy(conf = c)
    SGraph(node :: g.incompleteLeaves.tail, g.completeLeaves, g.completeNodes)
  }
}

case class RollbackStep[C, D](to: SNode[C, D], c: C) extends GraphStep[C, D] {
  def apply(g: SGraph[C, D]) = {
    def prune_?(n: SNode[C, D]) = n.tPath.startsWith(to.tPath)
    val node = to.copy(conf = c)
    val completeNodes1 = g.completeNodes.remove(prune_?)
    val completeLeaves1 = g.completeLeaves.remove(prune_?)
    val incompleteLeaves1 = g.incompleteLeaves.tail.remove(prune_?)
    SGraph(node :: incompleteLeaves1, completeLeaves1, completeNodes1)
  }
}