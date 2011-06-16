package mrsc

import scala.annotation.tailrec

/*!# Processing of complete graphs
 
 Graph builder knows only how to build a graph, but not what to do with this graph later.
 Processing of complete SC graphs is extracted into a separate abstraction.
 */

/*! An instance of a graph may be pruned, and a client may be interested in knowing that fact:
  so `GraphConsumer` receives `Some(graph)` when graph is completed and receives `None` 
  if the graph was pruned. 
 */
trait CoGraphConsumer[C, D, E] {
  val description: String
  def consume(graph: Option[CoGraph[C, D, E]]): Unit
}

/*!# Abstract machines
  
  An abstract machine represents the semantics of the object language 
  (more precisely, meta-semantics) through operations over SC graphs. 
  `Machine` corresponds to a novel (= non-deterministic) supercompiler.
 */
trait Machine[C, D, E] {
  def steps(pState: PState[C, D, E]): List[Command[C, D, E]]
}

/*!# SC cographs builders
 
 SC cographs builders implement the logic of constructing SC cographs step-by-step by invoking
 provided abstract machines. They build cographs - it is up to `CoGraphConsumer` how to use this 
 cograph further.
 */

/*! `MultiCoGraphBuilder` considers all steps returned by `MultiResultMachine` and proceeds with
  all possible variants.
 */
class CoGraphBuilder[C, D, E](machine: Machine[C, D, E], consumer: CoGraphConsumer[C, D, E]) {

  /*! It maintains a list of partial cographs ...
   */
  var partialCoGraphs: List[PartialCoGraph[C, D, E]] = null

  /*! ... starts with a one-element list of partial cographs ... 
   */
  def buildCoGraph(conf: C, info: E): Unit = {
    val startNode = CoNode[C, D, E](conf, info, null, None, Nil)
    partialCoGraphs = List(new PartialCoGraph(List(), List(startNode), Nil))
    loop()
  }

  /*! ... and loops
   */
  @tailrec
  private def loop(): Unit =
    partialCoGraphs match {
      /*! If `partialCoGraphs` is empty, then it just stops, */
      case Nil =>
      /*! otherwise it investigates the status of the first cograph: */
      case g :: gs =>
        g.activeLeaf match {
          /*! If the first cograph is completed, builder transforms it to the pure cograph
           and sends it to the consumer. 
          */
          case None =>
            // TODO: do we need to sort it??
            val orderedNodes = g.completeNodes.sortBy(_.coPath)(PathOrdering)
            val rootNode = orderedNodes.head
            val completed = CoGraph(rootNode, g.completeLeaves, orderedNodes)
            partialCoGraphs = gs
            consumer.consume(Some(completed))

          /*! If the first cograph is incomplete, then builder considers all variants suggested
           by `machine`:
           */
          case Some(leaf) =>
            partialCoGraphs = gs
            for (step <- machine.steps(g.pState)) step match {
              /*! informing `consumer` about pruning, if any */
              case DiscardGraph =>
                consumer.consume(None)
              /*! or adding new cograph to the pending list otherwise */
              case s =>
                partialCoGraphs = g.executeCommand(s) :: partialCoGraphs
            }
        }
        /*! and looping again. */
        loop()
    }

}