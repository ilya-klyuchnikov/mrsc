package mrsc.core

import scala.annotation.tailrec
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

/*!# Abstract machines
  
  An abstract machine represents the semantics of the object language 
  (more precisely, meta-semantics) through operations over SC graphs. 
  `Machine` corresponds to a novel (= non-deterministic) supercompiler.
 */

trait StepSignature[+C, +D, +E] {
  type G = Graph[C, D, E]
  type S = G => G
}

trait Machine[C, D, E] extends StepSignature[C, D, E] {
  def steps(g: G): List[S]
}

/*!# Processing of complete and unworkable graphs
 
 A graph generator knows only how to build a graph using a machine, but not what to do with this graph later.
 */

/*! This class produces iterators producing graphs by demand. */

case class GraphGenerator[C, D, E](machine: Machine[C, D, E], conf: C, info: E)
  extends Iterator[Graph[C, D, E]] {

  /*! It maintains a list of graphs
     * and starts with a one-element list of graphs. 
     */

  private var readyGs: Queue[Graph[C, D, E]] = Queue()
  private var gs: List[Graph[C, D, E]] = List(initial(conf, info))

  private def initial(c: C, e: E): Graph[C, D, E] = {
    val initialNode = Node[C, D, E](c, e, null, None, Nil)
    Graph(List(initialNode), Nil, Nil)
  }

  @tailrec
  private def normalize(): Unit =
    if (readyGs.isEmpty && !gs.isEmpty) {
      val pendingDelta = ListBuffer[Graph[C, D, E]]()
      val h = gs.head
      val newGs = for (buildStep <- machine.steps(h)) yield buildStep(h)
      for (g1 <- newGs)
        if (g1.isComplete || g1.isUnworkable) {
          readyGs.enqueue(g1)
        } else {
          pendingDelta += g1
        }
      gs = pendingDelta ++: gs.tail
      normalize()
    }

  def hasNext: Boolean = {
    normalize()
    !readyGs.isEmpty
  }

  def next(): Graph[C, D, E] = {
    if (!hasNext)
      throw new NoSuchElementException("no graph")
    readyGs.dequeue()
  }
}
