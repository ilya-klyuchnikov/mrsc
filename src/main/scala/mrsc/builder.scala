package mrsc

import scala.annotation.tailrec

/*! `PartialCoGraph[C, I]` is a central concept of MRSC. It represents a "work in progress".
 We know already processed part of an SC graph (`completeLeaves`, `completeNodes`) and a frontier 
 of incomplete part (`incompleteLeaves`).
 */
case class PartialCoGraph[C, D, E](
  completeLeaves: List[CoNode[C, D, E]],
  incompleteLeaves: List[CoNode[C, D, E]],
  complete: List[CoNode[C, D, E]]) {

  /*! `activeLeaf` is the vanguard of the incomplete part. It will be processed next.
   */
  val activeLeaf: Option[CoNode[C, D, E]] = incompleteLeaves.headOption

  /*! Partial state is exposed to SC machines. SC machine decides what step should be done next 
      based on `pState`.
   */
  val pState = PState(activeLeaf.getOrElse(null), complete)
}

/*! `PState` stands for partial state.
 Based on the current `PState`, SCP machine should decide what should be done next.
 */
case class PState[C, D, E](current: CoNode[C, D, E], complete: List[CoNode[C, D, E]])

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
  def steps(pState: PState[C, D, E]): List[Command[C, D, E]]
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
case class AddChildNodes[+C, +D, +E](val childNodes: List[ChildNode[C, D, E]]) extends Command[C, D, E]

/* Usually `SubStep` hides internals of driving.  */
case class ChildNode[+C, +D, +E](conf: C, driveInfo: D, extraInfo: E)

/*! `MFold` signals that there is a path to something similar to the current state in the past
 of the current SC Graph.
 */
case class MakeFold(val path: Path) extends Command[Nothing, Nothing, Nothing]

/*! The step `MPrune` means that the current graph should be discarded. In the case of 
 the "single-result" supercompilation it means failure (= no good result). In the case of
 multi-result supercompilation it means that we should continue with the next variant.   
 */
case object DiscardGraph extends Command[Nothing, Nothing, Nothing]

/*! `MReplace` is fixing the current state of the branch.
 */
case class ReplaceNode[C, D, E](val conf: C, val extraInfo: E) extends Command[C, D, E]

/*! `MRollback` is fixing the past `dangerous` state of the current branch.
 */
case class RollbackSubGraph[C, D, E](val subGraphRoot: CoNode[C, D, E],
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
            val orderedNodes = g.complete.sortBy(_.coPath)(PathOrdering)
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
                partialCoGraphs = executeCommand(g, s) :: partialCoGraphs
            }
        }
        /*! and looping again. */
        loop()
    }

  /*! The main logic of MRSC is here. 
     Step created by SC machine is "applied" to the current active leaf.
   */
  def executeCommand(g: PartialCoGraph[C, D, E], command: Command[C, D, E]): PartialCoGraph[C, D, E] = g.incompleteLeaves match {
    case active :: ls =>
      command match {
        /*! Just "completing" the current node - moving it to the complete part of the SC graph. 
         */
        case ConvertToLeaf =>
          PartialCoGraph(active :: g.completeLeaves, ls, active :: g.complete)
        /*! Replacing the configuration of the current node. 
           The main use case is the rebuilding (generalization) of the active node.
         */
        case ReplaceNode(conf, extra) =>
          val node = active.copy(conf = conf, extraInfo = extra)
          PartialCoGraph(g.completeLeaves, node :: ls, g.complete)
        /*! Just folding: creating a loopback and moving the node into the complete part 
            of the SC graph.  
         */
        case MakeFold(basePath) =>
          val node = active.copy(base = Some(basePath))
          PartialCoGraph(node :: g.completeLeaves, ls, node :: g.complete)
        /*! This step corresponds (mainly) to driving: adds children to the current node. Then
            current node is moved to the complete part and new children are moved into 
            the incomplete part. Also the (co-)path is calculated for any child node.
         */
        case AddChildNodes(subSteps) =>
          val deltaLeaves: List[CoNode[C, D, E]] = subSteps.zipWithIndex map {
            case (ChildNode(conf, dInfo, eInfo), i) =>
              val in = CoEdge(active, dInfo)
              CoNode(conf, eInfo, in, None, i :: active.coPath)
          }
          PartialCoGraph(g.completeLeaves, deltaLeaves ++ ls, active :: g.complete)
        /*! When doing rollback, we also prune all successors of the dangerous node. 
         */
        case RollbackSubGraph(dangNode, c, eInfo) =>
          def prune_?(n: CoNode[C, D, E]) = n.path.startsWith(dangNode.path)
          val node = dangNode.copy(conf = c, extraInfo = eInfo)
          val completeNodes1 = g.complete.remove(prune_?)
          val completeLeaves1 = g.completeLeaves.remove(prune_?)
          val incompleteLeaves1 = ls.remove(prune_?)
          PartialCoGraph(completeLeaves1, node :: incompleteLeaves1, completeNodes1)
        /*! A graph cannot prune itself - it should be performed by a builder.
         */
        case DiscardGraph =>
          throw new Error()
      }
    case _ =>
      throw new Error()
  }
}