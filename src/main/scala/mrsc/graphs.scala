package mrsc

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
       
 SC graphs in MRSC are used in three forms:
 
 * `Graph` - good for top-down traversals (a node knows about its outs).
 * `CoGraph` - good for easy bottom-up traversals (a node knows about in).
 * `PartialCoGraph` - good for using in multi-result supercompilation 
    (cograph consists of complete and incomplete parts, 
    and operation to add outs to incomplete nodes is cheap).
 
 The main idea here is to use functional (immutable) data structures in order to support
 multi-results composed of shared data.
 */

/*! The labeled directed edge. `N` is a destination node; `D` is driving info.
 */
case class Edge[C, D, E](node: Node[C, D, E], driveInfo: D)
case class CoEdge[C, D, E](coNode: CoNode[C, D, E], driveInfo: D)

/*! `Graph[C, D, E]`. 
  `C` (configuration) is a type of node label; 
  `D` (driving) is a type of edge label;
  `E` (extra information) is a type of extra label of a node. 
  Extra information may be seen as an additional "instrumentation" of SC graph.
 */
case class Graph[C, D, E](root: Node[C, D, E], leaves: Nodes[C, D, E]) {
  def get(path: Path): Node[C, D, E] = root.get(path)
  override def toString = root.toString
}

/*! `Node[C, D, E]` is a very simple and straightforward implementation of a node. 
 */
case class Node[C, D, E](
  conf: C,
  extraInfo: E,
  outs: List[Edge[C, D, E]],
  base: Loopback,
  path: Path) {

  lazy val coPath = path.reverse

  @tailrec
  final def get(relPath: Path): Node[C, D, E] = relPath match {
    case Nil => this
    case i :: rp => outs(i).node.get(rp)
  }

  val isLeaf = outs.isEmpty
  val isRepeat = base.isDefined
  
  override def toString = GraphPrettyPrinter.toString(this)
}

/*! `CoGraph[C, D, E]` is dual to `Graph[C, D, E]`. 
 It has additionally the list of all nodes (vertices).
 */
case class CoGraph[C, D, E](
  root: CoNode[C, D, E],
  leaves: CoNodes[C, D, E],
  nodes: CoNodes[C, D, E])

/*! `CoNode[C, D, E]` is dual to `Node[C, D, E]`. 
 */
case class CoNode[C, D, E](
  conf: C,
  extraInfo: E,
  in: CoEdge[C, D, E],
  base: Loopback,
  coPath: CoPath) {

  lazy val path = coPath.reverse

  val ancestors: List[CoNode[C, D, E]] =
    if (in == null) List() else in.coNode :: in.coNode.ancestors

  override def toString = conf.toString
}

/*! `PartialCoGraph[C, I]` is a central concept of MRSC. It represents a "work in progress".
 We know already processed part of an SC graph (`completeLeaves`, `completeNodes`) and a frontier 
 of incomplete part (`incompleteLeaves`).
 */
case class PartialCoGraph[C, D, E](
  completeLeaves: CoNodes[C, D, E],
  incompleteLeaves: CoNodes[C, D, E],
  completeNodes: CoNodes[C, D, E]) {

  /*! `activeLeaf` is the vanguard of the incomplete part. It will be processed next.
   */
  val activeLeaf: Option[CoNode[C, D, E]] = incompleteLeaves.headOption

  /*! Partial state is exposed to SC machines. SC machine decides what step should be done next 
      based on `pState`.
   */
  val pState = PState(activeLeaf.getOrElse(null), completeNodes)

  /*! The main logic of MRSC is here. 
     Step created by SC machine is "applied" to the current active leaf.
   */
  def addStep(step: Step[C, D, E]): PartialCoGraph[C, D, E] = incompleteLeaves match {
    case active :: ls =>
      step match {
        /*! Just "completing" the current node - moving it to the complete part of the SC graph. 
         */
        case Leaf =>
          PartialCoGraph(active :: completeLeaves, ls, active :: completeNodes)
        /*! Replacing the configuration of the current node. 
           The main use case is the rebuilding (generalization) of the active node.
         */
        case Replace(conf, extra) =>
          val node = active.copy(conf = conf, extraInfo = extra)
          PartialCoGraph(completeLeaves, node :: ls, completeNodes)
        /*! Just folding: creating a loopback and moving the node into the complete part 
            of the SC graph.  
         */
        case Fold(basePath) =>
          val node = active.copy(base = Some(basePath))
          PartialCoGraph(node :: completeLeaves, ls, node :: completeNodes)
        /*! This step corresponds (mainly) to driving: adds children to the current node. Then
            current node is moved to the complete part and new children are moved into 
            the incomplete part. Also the (co-)path is calculated for any child node.
         */
        case Forest(subSteps) =>
          val deltaLeaves: CoNodes[C, D, E] = subSteps.zipWithIndex map {
            case (SubStep(conf, dInfo, eInfo), i) =>
              val in = CoEdge(active, dInfo)
              CoNode(conf, eInfo, in, None, i :: active.coPath)
          }
          PartialCoGraph(completeLeaves, deltaLeaves ++ ls, active :: completeNodes)
        /*! When doing rollback, we also prune all successors of the dangerous node. 
         */
        case Rollback(dangNode, c, eInfo) =>
          def prune_?(n: CoNode[C, D, E]) = n.path.startsWith(dangNode.path)
          val node = dangNode.copy(conf = c, extraInfo = eInfo)
          val completeNodes1 = completeNodes.remove(prune_?)
          val completeLeaves1 = completeLeaves.remove(prune_?)
          val incompleteLeaves1 = ls.remove(prune_?)
          PartialCoGraph(completeLeaves1, node :: incompleteLeaves1, completeNodes1)
        /*! A graph cannot prune itself - it should be performed by a builder.
         */
        case Prune =>
          throw new Error()
      }
    case _ =>
      throw new Error()
  }
}

/*! `PState` stands for partial state.
 Based on the current `PState`, SCP machine should decide what should be done next.
 */
case class PState[C, D, E](val node: CoNode[C, D, E], val completeNodes: CoNodes[C, D, E])
