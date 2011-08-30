package mrsc.core

import scala.annotation.tailrec
/*! # SC Graph Abstraction
 
 At the heart of MRSC is a mini-framework for manipulating SC graphs.
 In MRSC SC Graph is a special kind of graph that can be seen as a tree (skeleton), 
 some leaves of which can have loopbacks.
 Note that loopbacks can start only from leaves. 
 
 In some sense, SC Graph is similar to accessible
 pointed graphs (APGs) used in the theory of non-well-founded sets.
 
 Here is the simplest SC graph. There are two loopbacks: j⇢c and h⇢d. 
 There is also a concept of a path. The path to the node g is [0,0,1].

         . c
        .  ↓
      .    d
     .     ↓  .  
    .      e   . 
    .   ↙ ↓ ↘ .  
    .  f   g   h
     . ↓
       j
       
 SC graphs in MRSC are used in two representations:
 
 * `Graph` - good for easy bottom-up traversals (a node knows about in) and
    for using in multi-result supercompilation 
    (graph consists of complete and incomplete parts, 
    and operation to add outs to incomplete nodes is cheap).
 * `TGraph` - good for top-down traversals (a node knows about its outs).
 
 The main idea here is to use functional (immutable) data structures in order to support
 multi-results composed of shared data.
 */

/*! `Node[+C, +D, +E]` is dual to `TNode[C, D, E]`. 
 */
case class Node[+C, +D, +E](
  conf: C,
  extraInfo: E,
  in: Edge[C, D, E],
  back: Option[Path],
  path: Path) {

  lazy val tPath = path.reverse

  val ancestors: List[Node[C, D, E]] =
    if (in == null) List() else in.node :: in.node.ancestors

  override def toString = conf.toString
}

/*! `Edge[+C, +D, +E]` is dual to `TEdge[C, D, E]`. 
 */
case class Edge[+C, +D, +E](node: Node[C, D, E], driveInfo: D)

/*! `Graph[C, D, E]` is a core data structure in MRSC.
 * It may represent (1) a "work in progress" (2) a completed graph and
 * (3) unfinished, and yet unworkable graph.
 * We know already processed part of an SC graph
 * (`completeLeaves`, `completeNodes`) and a frontier 
 * of incomplete part (`incompleteLeaves`).
 * 
 *`C` (configuration) is a type of node label; 
 *`D` (driving) is a type of edge label (driving info);
 *`E` (extra information) is a type of extra label of a node (extra info). 
 * Extra information may be seen as an additional "instrumentation" of SC graph.
 */
case class Graph[+C, +D, +E](
  incompleteLeaves: List[Node[C, D, E]],
  completeLeaves: List[Node[C, D, E]],
  completeNodes: List[Node[C, D, E]],
  isUnworkable: Boolean = false) {

  /*! `isUnworkable` = is not good for further processing (for some reason).
   *  'isComplete` = is finished, there is nothing to do.
   */
  val isComplete = incompleteLeaves.isEmpty

  /*! `current` is the vanguard of the incomplete part. It will be processed next.
   */
  val current = if (isComplete) null else incompleteLeaves.head
}

/*!# Abstract steps
   Under the hood an abstract machine deals with some kind of semantics of the language.
   Low-level operations should be translated into high-level abstract operations (or messages) 
   over SC graphs.
*/

/*! Transformations performed over Graphs by driving
 *  (and some other parts of the supercompiler?).
 */

trait MachineSteps[C, D, E] extends StepSignature[C, D, E] {

  /*! Marking the graph as unworkable
   */
  def toUnworkable: S =
    g => g.copy(isUnworkable = true)
    
  /*! Just "completing" the current node - moving it to the complete part of the SC graph. 
  */
  def completeCurrentNode: S =
    g => Graph(g.incompleteLeaves.tail, g.current :: g.completeLeaves,
      g.current :: g.completeNodes)

  /*! This step corresponds (mainly) to driving: adds children to the current node. Then
   *  current node is moved to the complete part and new children are moved into 
   *  the incomplete part. Also the (co-)path is calculated for any child node.
   */
  def addChildNodes(ns: List[(C, D, E)]): S =
    g => {
      val deltaLeaves: List[Node[C, D, E]] = ns.zipWithIndex map {
        case ((conf, dInfo, eInfo), i) =>
          val in = Edge(g.current, dInfo)
          Node(conf, eInfo, in, None, i :: g.current.path)
      }
      // Now it is depth-first traversal. If you change 
      // deltaLeaves ++ ls -> ls ++ deltaLeaves,
      // you will have breadth-first traversal
      Graph(deltaLeaves ++ g.incompleteLeaves.tail, g.completeLeaves,
        g.current :: g.completeNodes)
    }

  /*! Just folding: creating a loopback and moving the node into the complete part 
   *  of the SC graph.  
   */
  def fold(backNode: Node[C, D, E]): S =
    g => {
      val node = g.current.copy(back = Some(backNode.path))
      Graph(g.incompleteLeaves.tail, node :: g.completeLeaves, node :: g.completeNodes)
    }

  /*! Replacing the configuration of the current node. 
   *  The main use case is the rebuilding (generalization) of the active node.
   */
  def rebuild(conf: C, extra: E): S =
    g => {
      val node = g.current.copy(conf = conf, extraInfo = extra)
      Graph(node :: g.incompleteLeaves.tail, g.completeLeaves, g.completeNodes)
    }

  /*! When doing rollback, we also prune all successors of the dangerous node. 
   */
  def rollback(dangNode: Node[C, D, E], c: C, info: E): S =
    g => {
      def prune_?(n: Node[C, D, E]) = n.tPath.startsWith(dangNode.tPath)
      val node = dangNode.copy(conf = c, extraInfo = info)
      val completeNodes1 = g.completeNodes.remove(prune_?)
      val completeLeaves1 = g.completeLeaves.remove(prune_?)
      val incompleteLeaves1 = g.incompleteLeaves.tail.remove(prune_?)
      Graph(node :: incompleteLeaves1, completeLeaves1, completeNodes1)
    }
}

