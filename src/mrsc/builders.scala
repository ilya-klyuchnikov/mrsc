package mrsc

import scala.annotation.tailrec

class SingleCoGraphBuilder[C, I](machine: SingleMachine[C, I]) {
  def buildCoGraph(conf: C, info: I): CoGraph[C, I] = {
    val rootNode = CoNode[C, I](conf, info, null, None, Nil)
    val initialCoGraph = new PartialCoGraph(List(), List(rootNode), Nil)
    val finalCoGraph = build(initialCoGraph)
    val orderedNodes = finalCoGraph.completeNodes.sortBy(_.coPath)(PathOrdering)
    CoGraph(rootNode, finalCoGraph.completeLeaves, orderedNodes)
  }

  @tailrec
  private def build(g: PartialCoGraph[C, I]): PartialCoGraph[C, I] = g.activeLeaf match {
    case None => g
    case Some(leaf) => build(g.addStep(machine.makeStep(g.pState)))
  }
}

// Sinlge-thread builder
class MultiCoGraphBuilder[C, I](machine: MultiMachine[C, I], consumer: CoGraphConsumer[C, I]) {

  var partialGraphs: List[PartialCoGraph[C, I]] = null
  var rootNode: CoNode[C, I] = null

  def buildCoGraph(conf: C, info: I): Unit = {
    rootNode = CoNode[C, I](conf, info, null, None, Nil)
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