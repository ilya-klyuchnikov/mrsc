package mrsc.core

/*! `Graph[C, D, E]` is a central concept of MRSC.
 * It may represent (1) a "work in progress" (2) a completed graph and
 * (3) and unfinished yet unworkable graph.
 * We know already processed part of an SC graph
 * (`completeLeaves`, `completeNodes`) and a frontier 
 * of incomplete part (`incompleteLeaves`).
 */
case class Graph[C, D, E](
  incompleteLeaves: List[Node[C, D, E]],
  completeLeaves: List[Node[C, D, E]],
  completeNodes: List[Node[C, D, E]],
  isUnworkable: Boolean = false) {

  /*! `isUnworkable` = is not good for further processing (for some reason).
   *  'isComplete` = is finished, there is nothing to do.
   */
  val isComplete = incompleteLeaves.isEmpty 
  def toUnworkable() = this.copy(isUnworkable = true)
  
  /*! `current` is the vanguard of the incomplete part. It will be processed next.
   */
  val current = if (isComplete) null else incompleteLeaves.head
  
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
  def completeCurrentNode() : Graph[C, D, E] = {
    Graph(incompleteLeaves.tail, current :: completeLeaves, current :: completeNodes)
  }
  /*! This step corresponds (mainly) to driving: adds children to the current node. Then
   *  current node is moved to the complete part and new children are moved into 
   *  the incomplete part. Also the (co-)path is calculated for any child node.
   */
  def addChildNodes(ns: List[(C, D, E)]) = {
    val deltaLeaves: List[Node[C, D, E]] = ns.zipWithIndex map {
      case ((conf, dInfo, eInfo), i) =>
        val in = Edge(current, dInfo)
        Node(conf, eInfo, in, None, i :: current.path)
    }
    // Now it is depth-first traversal. If you change 
    // deltaLeaves ++ ls -> ls ++ deltaLeaves,
    // you will have breadth-first traversal
    Graph(deltaLeaves ++ incompleteLeaves.tail, completeLeaves, current :: completeNodes)
  }
  /*! Just folding: creating a loopback and moving the node into the complete part 
   *  of the SC graph.  
   */
  def fold(backPath: Path): Graph[C, D, E] = {
    val node = current.copy(back = Some(backPath))
    Graph(incompleteLeaves.tail, node :: completeLeaves, node :: completeNodes)
  }
  /*! Replacing the configuration of the current node. 
   *  The main use case is the rebuilding (generalization) of the active node.
   */
  def rebuild(conf: C, extra: E): Graph[C, D, E] = {
    val node = current.copy(conf = conf, extraInfo = extra)
    Graph(node :: incompleteLeaves.tail, completeLeaves, completeNodes)
  }
  /*! When doing rollback, we also prune all successors of the dangerous node. 
   */
  def rollback(dangNode: Node[C, D, E], c: C, eInfo: E) = {
    def prune_?(n: Node[C, D, E]) = n.tPath.startsWith(dangNode.tPath)
    val node = dangNode.copy(conf = c, extraInfo = eInfo)
    val completeNodes1 = completeNodes.remove(prune_?)
    val completeLeaves1 = completeLeaves.remove(prune_?)
    val incompleteLeaves1 = incompleteLeaves.tail.remove(prune_?)
    Graph(node :: incompleteLeaves1, completeLeaves1, completeNodes1)
  }
}

/*!# Processing of complete graphs
 
 Graph builder knows only how to build a graph, but not what to do with this graph later.
 Processing of complete SC graphs is extracted into a separate abstraction.
 */

trait GraphConsumer[C, D, E, R] {
  def consume(graph: Graph[C, D, E]): Unit
  def buildResult(): R
}

/*!# Abstract machines
  
  An abstract machine represents the semantics of the object language 
  (more precisely, meta-semantics) through operations over SC graphs. 
  `Machine` corresponds to a novel (= non-deterministic) supercompiler.
 */
trait Machine[C, D, E] {
  type G = Graph[C, D, E]
  def steps(g: G): List[G]
}

/*! This class is essentially an iterator producing graphs by demand. */

case class GraphProducer[C, D, E](conf: C, info: E, machine: Machine[C, D, E]) {

  /*! It maintains a list of graphs
   * and starts with a one-element list of graphs. 
   */

  private var gs: List[Graph[C, D, E]] = List(start(conf, info))

  private def start(c: C, e: E): Graph[C, D, E] = {
    val startNode = Node[C, D, E](c, e, null, None, Nil)
    new Graph(List(startNode), Nil, Nil)
  }

  private def normalize() {
    while (true) {
      if (gs.isEmpty)
        return
      val g = gs.head
      if (g.isComplete || g.isUnworkable)
        return
      gs = machine.steps(g) ++ gs.tail
    }
  }

  def hasNext: Boolean = {
    normalize()
    !gs.isEmpty
  }

  def next() : Graph[C, D, E] = {
    if (!hasNext)
      throw new NoSuchElementException("no graph")
    val g = gs.head
    gs = gs.tail
    g
  }
}

/*! This class is defined only to mimic the behavior of the old GraphBuilder.
 * Normally, it is up to consumer to drive the producer and to decide when to stop. 
 */

class GraphBuilder[C, D, E](machine: Machine[C, D, E], consumer: GraphConsumer[C, D, E, _]) {

  def buildGraphs(conf: C, info: E): Unit = {
    val producer = new GraphProducer[C, D, E](conf, info, machine)
    while (producer.hasNext) {
      consumer.consume(producer.next())
    }
  }
}
