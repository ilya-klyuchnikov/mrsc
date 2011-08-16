package mrsc.core

import scala.annotation.tailrec

/*! `PartialCoGraph[C, D, E]` is a central concept of MRSC. It represents a "work in progress".
 We know already processed part of an SC graph (`completeLeaves`, `completeNodes`) and a frontier 
 of incomplete part (`incompleteLeaves`).
 */
case class PartialCoGraph[C, D, E](
  incompleteLeaves: List[CoNode[C, D, E]],
  completeLeaves: List[CoNode[C, D, E]],
  complete: List[CoNode[C, D, E]]) {

  /*! `activeLeaf` is the vanguard of the incomplete part. It will be processed next.
   */
  val activeLeaf: Option[CoNode[C, D, E]] = incompleteLeaves.headOption
  val current = activeLeaf.getOrElse(null)

  /*! Transformations performed over graphs by driving
   *  (and some other parts of the supercompiler?)
   *  Perhaps, they might be exposed via a trait?
   */

  /*! Just "completing" the current node - moving it to the complete part of the SC graph. 
   */
  def convertToLeaf() : PartialCoGraph[C, D, E] = {
    PartialCoGraph(incompleteLeaves.tail, current :: completeLeaves, current :: complete)
  }
  /*! This step corresponds (mainly) to driving: adds children to the current node. Then
   *  current node is moved to the complete part and new children are moved into 
   *  the incomplete part. Also the (co-)path is calculated for any child node.
   */
  def addChildNodes(ns: List[(C, D, E)]) = {
    val deltaLeaves: List[CoNode[C, D, E]] = ns.zipWithIndex map {
      case ((conf, dInfo, eInfo), i) =>
        val in = CoEdge(current, dInfo)
        CoNode(conf, eInfo, in, None, i :: current.coPath)
    }
    // Now it is depth-first traversal. If you change 
    // deltaLeaves ++ ls -> ls ++ deltaLeaves,
    // you will have breadth-first traversal
    PartialCoGraph(deltaLeaves ++ incompleteLeaves.tail, completeLeaves, current :: complete)
  }
  /*! Just folding: creating a loopback and moving the node into the complete part 
   *  of the SC graph.  
   */
  def fold(basePath: CoPath): PartialCoGraph[C, D, E] = {
    val node = current.copy(base = Some(basePath))
    PartialCoGraph(incompleteLeaves.tail, node :: completeLeaves, node :: complete)
  }
  /*! Replacing the configuration of the current node. 
   *  The main use case is the rebuilding (generalization) of the active node.
   */
  def rebuild(conf: C, extra: E): PartialCoGraph[C, D, E] = {
    val node = current.copy(conf = conf, extraInfo = extra)
    PartialCoGraph(node :: incompleteLeaves.tail, completeLeaves, complete)
  }
  /*! When doing rollback, we also prune all successors of the dangerous node. 
   */
  def rollback(dangNode: CoNode[C, D, E], c: C, eInfo: E) = {
    def prune_?(n: CoNode[C, D, E]) = n.path.startsWith(dangNode.path)
    val node = dangNode.copy(conf = c, extraInfo = eInfo)
    val completeNodes1 = complete.remove(prune_?)
    val completeLeaves1 = completeLeaves.remove(prune_?)
    val incompleteLeaves1 = incompleteLeaves.tail.remove(prune_?)
    PartialCoGraph(node :: incompleteLeaves1, completeLeaves1, completeNodes1)
  }

}

/*!# Processing of complete graphs
 
 Graph builder knows only how to build a graph, but not what to do with this graph later.
 Processing of complete SC graphs is extracted into a separate abstraction.
 */

/*! An instance of a graph may be pruned, and a client may be interested in knowing that fact:
  so `GraphConsumer` receives `Some(graph)` when graph is completed and receives `None` 
  if the graph was pruned. 
 */
trait CoGraphConsumer[C, D, E, R] {
  def consume(graph: Option[CoGraph[C, D, E]]): Unit
  def buildResult(): R
}

/*!# Abstract machines
  
  An abstract machine represents the semantics of the object language 
  (more precisely, meta-semantics) through operations over SC graphs. 
  `Machine` corresponds to a novel (= non-deterministic) supercompiler.
 */
trait Machine[C, D, E] {
  type CG = PartialCoGraph[C, D, E]
  type CMD = Command[C, D, E]
  def steps(coGraph: CG): List[CMD]
}

/*!# Abstract steps
  
 Under the hood an abstract machine deals with some kind of semantics of the language.
 Low-level operations should be translated into high-level abstract operations (or messages) 
 over SC graphs.
 */
sealed trait Command[+C, +D, +E]

/*! The step `MMakeLeaf` means that the current branch of the graph in focus is
   a terminal node (leaf).
 */
case object ConvertToLeaf extends Command[Nothing, Nothing, Nothing]

/*! `MAddForest` corresponds to development of current branch of the graph (driving in 90%).
 Development is divided into several `subSteps`.
 */
case class AddChildNodes[C, D, E](ns: List[(C, D, E)]) extends Command[C, D, E]

/*! `MFold` signals that there is a path to something similar to the current state in the past
 of the current SC Graph.
 */
case class Fold(coPath: CoPath) extends Command[Nothing, Nothing, Nothing]

/*! The step `MPrune` means that the current graph should be discarded. In the case of 
 the "single-result" supercompilation it means failure (= no good result). In the case of
 multi-result supercompilation it means that we should continue with the next variant.   
 */
case object Discard extends Command[Nothing, Nothing, Nothing]

/*! `MReplace` is fixing the current state of the branch.
 */
case class Rebuild[C, D, E](conf: C, extraInfo: E) extends Command[C, D, E]

/*! `MRollback` is fixing the past `dangerous` state of the current branch.
 */
case class Rollback[C, D, E](to: CoNode[C, D, E],
  val conf: C, val extraInfo: E) extends Command[C, D, E]

/*!# SC cographs builders
 
 SC cographs builders implement the logic of constructing SC cographs step-by-step by invoking
 provided abstract machines. They build cographs - it is up to `CoGraphConsumer` how to use this 
 cograph further.
 */

/*! `MultiCoGraphBuilder` considers all steps returned by `MultiResultMachine` and proceeds with
  all possible variants.
 */
class CoGraphBuilder[C, D, E](machine: Machine[C, D, E], consumer: CoGraphConsumer[C, D, E, _]) {
  import CoGraphBuilder._

  /*! It maintains a list of partial cographs ...
   */
  var partialCoGraphs: List[PartialCoGraph[C, D, E]] = null

  /*! ... starts with a one-element list of partial cographs ... 
   */
  def buildCoGraph(conf: C, info: E): Unit = {
    partialCoGraphs = List(start(conf, info))
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
            val completed = complete(g)
            partialCoGraphs = gs
            consumer.consume(Some(completed))

          /*! If the first cograph is incomplete, then builder considers all variants suggested
           by `machine`:
           */
          case Some(leaf) =>
            partialCoGraphs = gs
            for (step <- machine.steps(g)) step match {
              /*! informing `consumer` about pruning, if any */
              case Discard =>
                consumer.consume(None)
              /*! or adding new cograph to the pending list otherwise */
              case s =>
                partialCoGraphs = executeCommand(g, s) :: partialCoGraphs
            }
        }
        /*! and looping again. */
        loop()
    }
}

/*! The main logic of MRSC is here. 
   Step created by SC machine is "applied" to the current active leaf.
*/
object CoGraphBuilder {
  def start[C, D, E](c: C, e: E): PartialCoGraph[C, D, E] = {
    val startNode = CoNode[C, D, E](c, e, null, None, Nil)
    new PartialCoGraph(List(startNode), Nil, Nil)
  }

  def executeCommand[C, D, E](g: PartialCoGraph[C, D, E], command: Command[C, D, E]): PartialCoGraph[C, D, E] = g.incompleteLeaves match {
    case active :: ls =>
      command match {
        case ConvertToLeaf =>
          g.convertToLeaf()
        case Rebuild(conf, extra) =>
          g.rebuild(conf, extra)
        case Fold(basePath) =>
          g.fold(basePath)
        case AddChildNodes(ns) =>
          g.addChildNodes(ns)
        case Rollback(dangNode, c, eInfo) =>
          g.rollback(dangNode, c, eInfo)
         /*! A graph cannot prune itself - it should be performed by a builder.
         */
        case Discard =>
          throw new Error()
      }
    case _ =>
      throw new Error()
  }

  def complete[C, D, E](g: PartialCoGraph[C, D, E]): CoGraph[C, D, E] = {
    val orderedNodes = g.complete.sortBy(_.coPath)(PathOrdering)
    val rootNode = orderedNodes.head
    CoGraph(rootNode, g.completeLeaves, orderedNodes)
  }
}