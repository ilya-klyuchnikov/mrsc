package mrsc.core

import scala.annotation.tailrec
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

/*!# Abstract machines
  
  An abstract machine represents the semantics of the object language 
  (more precisely, meta-semantics) through operations over SC graphs. 
  `Machine` corresponds to a novel (= non-deterministic) supercompiler.
 */

trait Machine[C, D] {
  type N = SNode[C, D]
  type G = SGraph[C, D]
  type S = GraphStep[C, D]
  def steps(g: G): List[S]
}

/*!# Abstract steps
   Under the hood an abstract machine deals with some kind of semantics of the language.
   Low-level operations should be translated into high-level abstract operations (or messages) 
   over SC graphs.
*/
sealed trait GraphStep[C, D]
case class CompleteCurrentNodeStep[C, D] extends GraphStep[C, D]
case class AddChildNodesStep[C, D](ns: List[(C, D)]) extends GraphStep[C, D]
case class FoldStep[C, D](baseNode: SNode[C, D]) extends GraphStep[C, D]
case class RebuildStep[C, D](c: C) extends GraphStep[C, D]
case class RollbackStep[C, D](to: SNode[C, D], c: C) extends GraphStep[C, D]

/*!# Generating graphs.
 
 A graph generator knows only how to build a graph using a machine, but not what to do with this graph later.
 */

/*! This class produces iterators producing graphs by demand. */

case class GraphGenerator[C, D](machine: Machine[C, D], conf: C)
  extends Iterator[SGraph[C, D]] {

  /*! It maintains a list of graphs
     * and starts with a one-element list of graphs. 
     */

  private var completeGs: Queue[SGraph[C, D]] = Queue()
  private var gs: List[SGraph[C, D]] = List(initial(conf))

  private def initial(c: C): SGraph[C, D] = {
    val initialNode = SNode[C, D](c, null, None, Nil)
    SGraph(List(initialNode), Nil, Nil)
  }

  @tailrec
  private def normalize(): Unit =
    if (completeGs.isEmpty && !gs.isEmpty) {
      val pendingDelta = ListBuffer[SGraph[C, D]]()
      val h = gs.head
      val newGs = machine.steps(h) map { GraphGenerator.executeStep(_, h) }
      for (g1 <- newGs)
        if (g1.isComplete) {
          completeGs.enqueue(g1)
        } else {
          pendingDelta += g1
        }
      gs = pendingDelta ++: gs.tail
      normalize()
    }

  def hasNext: Boolean = {
    normalize()
    !completeGs.isEmpty
  }

  def next(): SGraph[C, D] = {
    if (!hasNext) throw new NoSuchElementException("no graph")
    completeGs.dequeue()
  }
}

object GraphGenerator {
  def executeStep[C, D](step: GraphStep[C, D], g: SGraph[C, D]): SGraph[C, D] = step match {
    case CompleteCurrentNodeStep() =>
      SGraph(g.incompleteLeaves.tail, g.current :: g.completeLeaves, g.current :: g.completeNodes)
    case AddChildNodesStep(ns) =>
      val deltaLeaves: List[SNode[C, D]] = ns.zipWithIndex map {
        case ((conf, dInfo), i) =>
          val in = SEdge(g.current, dInfo)
          SNode(conf, in, None, i :: g.current.sPath)
      }
      SGraph(deltaLeaves ++ g.incompleteLeaves.tail, g.completeLeaves, g.current :: g.completeNodes)
    case FoldStep(baseNode) =>
      val node = g.current.copy(base = Some(baseNode.sPath))
      SGraph(g.incompleteLeaves.tail, node :: g.completeLeaves, node :: g.completeNodes)
    case RebuildStep(c) =>
      val node = g.current.copy(conf = c)
      SGraph(node :: g.incompleteLeaves.tail, g.completeLeaves, g.completeNodes)
    case RollbackStep(to, c) =>
      def prune_?(n: SNode[C, D]) = n.tPath.startsWith(to.tPath)
      val node = to.copy(conf = c)
      val completeNodes1 = g.completeNodes.remove(prune_?)
      val completeLeaves1 = g.completeLeaves.remove(prune_?)
      val incompleteLeaves1 = g.incompleteLeaves.tail.remove(prune_?)
      SGraph(node :: incompleteLeaves1, completeLeaves1, completeNodes1)
  }
}