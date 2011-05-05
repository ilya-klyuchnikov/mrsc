package mrsc

import scala.annotation.tailrec

class SingleCoGraphBuilder[C, D, E](machine: SingleResultMachine[C, D, E]) {
  def buildCoGraph(conf: C, info: E): CoGraph[C, D, E] = {
    val rootNode_ = CoNode[C, D, E](conf, info, null, None, Nil)
    val initialCoGraph = new PartialCoGraph(List(), List(rootNode_), Nil)
    val finalCoGraph = build(initialCoGraph)
    val orderedNodes = finalCoGraph.completeNodes.sortBy(_.coPath)(PathOrdering)
    val rootNode = orderedNodes.head
    CoGraph(rootNode, finalCoGraph.completeLeaves, orderedNodes)
  }

  @tailrec
  private def build(g: PartialCoGraph[C, D, E]): PartialCoGraph[C, D, E] = g.activeLeaf match {
    case None => g
    case Some(leaf) => build(g.addStep(machine.makeStep(g.pState)))
  }
}

// Sinlge-thread builder
class MultiCoGraphBuilder[C, D, E](machine: MultiResultMachine[C, D, E], consumer: CoGraphConsumer[C, D, E]) {

  var partialGraphs: List[PartialCoGraph[C, D, E]] = null
  var rootNode: CoNode[C, D, E] = null

  def buildCoGraph(conf: C, info: E): Unit = {
    rootNode = CoNode[C, D, E](conf, info, null, None, Nil)
    partialGraphs = List(new PartialCoGraph(List(), List(rootNode), Nil))
    loop()
  }

  // loop while there are graphs
  @tailrec
  private def loop(): Unit = {
    partialGraphs match {

      case Nil => // cool! everything is done!
      case g :: gs =>
        g.activeLeaf match {
          // the current graph is completed
          case None => {
            val orderedNodes = g.completeNodes.sortBy(_.coPath)(PathOrdering)
            val completed = CoGraph(rootNode, g.completeLeaves, orderedNodes)
            partialGraphs = gs
            consumer.consume(Some(completed))
          }
          case Some(leaf) =>
            var newGs = gs
            for (step <- machine.makeSteps(g.pState)) step match {
              case MPrune =>
                consumer.consume(None)
              case s =>
                val newG = g.addStep(s)
                newGs = newG :: newGs
            }
            partialGraphs = newGs
        }
        loop()
    }
  }

}