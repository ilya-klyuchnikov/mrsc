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
  type G = SGraph[C, D]
  type S = GraphStep[C, D]
  def steps(g: G): List[S] 
}

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
      val newGs = machine.steps(h) map {_(h)}
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
