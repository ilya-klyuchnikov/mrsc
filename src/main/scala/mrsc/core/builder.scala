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

  /*! `current` is the vanguard of the incomplete part. It will be processed next.
   */
  val current = if (incompleteLeaves.isEmpty) null else incompleteLeaves.head

  /*! Transformations performed over graphs by driving
   *  (and some other parts of the supercompiler?)
   *  Perhaps, they might be exposed via a trait?
   */

  /*!# Abstract steps
     Under the hood an abstract machine deals with some kind of semantics of the language.
     Low-level operations should be translated into high-level abstract operations (or messages) 
     over SC graphs.
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

  def toCoGraph(): CoGraph[C, D, E] = {
    val orderedNodes = complete.sortBy(_.coPath)(PathOrdering)
    val rootNode = orderedNodes.head
    CoGraph(rootNode, completeLeaves, orderedNodes)
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
  def steps(g: CG): List[CG]
}

/*! This class is essentially an iterator producing cographs by demand. */

case class CoGraphProducer[C, D, E](conf: C, info: E, machine: Machine[C, D, E]) {

  /*! It maintains a list of partial cographs
   * and starts with a one-element list of partial cographs. 
   */

  private var gs: List[PartialCoGraph[C, D, E]] = List(start(conf, info))

  private def start(c: C, e: E): PartialCoGraph[C, D, E] = {
    val startNode = CoNode[C, D, E](c, e, null, None, Nil)
    new PartialCoGraph(List(startNode), Nil, Nil)
  }

  private def normalize() {
    while (true) {
      if (gs.isEmpty)
        return
      val g = gs.head
      if (g == null || g.current == null)
        return
      gs = machine.steps(g) ++ gs.tail
    }
  }

  def hasNext(): Boolean = {
    normalize()
    !gs.isEmpty
  }

  def next : PartialCoGraph[C, D, E] = {
    if (!hasNext)
      throw new NoSuchElementException("no cograph")
    val g = gs.head
    gs = gs.tail
    g
  }
}

/*! This class is defined only to mimic the behavior of the old CoGraphBuilder.
 * Normally, it is up to consumer to drive the producer and to decide when to stop. 
 */

class CoGraphBuilder[C, D, E](machine: Machine[C, D, E], consumer: CoGraphConsumer[C, D, E, _]) {

  def buildCoGraph(conf: C, info: E): Unit = {
    val producer = new CoGraphProducer[C, D, E](conf, info, machine)
    while (producer.hasNext) {
      val g = producer.next
      if (g == null)
        /*! informing `consumer` that a graph has been discarded. */
        consumer.consume(None)
      else
        consumer.consume(Some(g.toCoGraph()))
    }
  }
}
