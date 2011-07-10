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
  `D` (driving) is a type of edge label (driving info);
  `E` (extra information) is a type of extra label of a node (extra info). 
  Extra information may be seen as an additional "instrumentation" of SC graph.
 */
case class Graph[C, D, E](root: Node[C, D, E], leaves: List[Node[C, D, E]]) {
  def get(path: Path): Node[C, D, E] = root.get(path)
  override def toString = root.toString
}

/*! `Node[C, D, E]` is a very simple and straightforward implementation of a node. 
 */
case class Node[C, D, E](
  conf: C,
  extraInfo: E,
  outs: List[Edge[C, D, E]],
  base: Option[Path],
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
  leaves: List[CoNode[C, D, E]],
  nodes: List[CoNode[C, D, E]])

/*! `CoNode[C, D, E]` is dual to `Node[C, D, E]`. 
 */
case class CoNode[C, D, E](
  conf: C,
  extraInfo: E,
  in: CoEdge[C, D, E],
  base: Option[CoPath],
  coPath: CoPath) {

  lazy val path = coPath.reverse

  val ancestors: List[CoNode[C, D, E]] =
    if (in == null) List() else in.coNode :: in.coNode.ancestors

  override def toString = conf.toString
}

/*! Auxiliary data for transposing a cograph into a graph.
 */
case class Tmp[C, D, E](node: Node[C, D, E], in: CoEdge[C, D, E])

/*! A transformer of cographs into graphs.
 */
object Transformations {
  /*! Transposition is done in the following simple way. Nodes are grouped according to the 
   levels (the root is 0-level). Then graphs are produced from in bottom-up fashion.
   */
  def transpose[C, D, E](coGraph: CoGraph[C, D, E]): Graph[C, D, E] = {
    val leafPathes = coGraph.leaves.map(_.coPath)
    val levels = coGraph.nodes.groupBy(_.coPath.length).toList.sortBy(_._1).map(_._2)
    val (tNodes, tLeaves) = subTranspose(levels, leafPathes)
    val nodes = tNodes map { _.node }
    val leaves = tLeaves map { _.node }
    return Graph(nodes(0), leaves)
  }

  // sub-transposes cogpaph into graph level-by-level
  private def subTranspose[C, D, E](
    nodes: List[List[CoNode[C, D, E]]],
    leaves: List[Path]): (List[Tmp[C, D, E]], List[Tmp[C, D, E]]) =
    nodes match {
      case Nil =>
        (Nil, Nil)

      // leaves only??
      case ns1 :: Nil =>
        val tmpNodes: List[Tmp[C, D, E]] = ns1 map { n =>
          val node = Node[C, D, E](n.conf, n.extraInfo, Nil, n.base, n.path)
          Tmp(node, n.in)
        }
        val tmpLeaves = tmpNodes.filter { tmp =>
          leaves.contains(tmp.node.coPath)
        }
        (tmpNodes, tmpLeaves)

      case ns1 :: ns => {
        val (allCh, leaves1) = subTranspose(ns, leaves)
        val allchildren = allCh.groupBy { _.node.coPath.tail }
        val tmpNodes = ns1 map { n =>
          val children: List[Tmp[C, D, E]] = allchildren.getOrElse(n.coPath, Nil)
          val edges = children map { tmp => Edge(tmp.node, tmp.in.driveInfo) }
          val node = new Node(n.conf, n.extraInfo, edges, n.base, n.path)
          Tmp(node, n.in)
        }
        val tmpLeaves = tmpNodes.filter { tmp => leaves.contains(tmp.node.coPath) }
        (tmpNodes, tmpLeaves ++ leaves1)
      }
    }
}

/*! Ad Hoc console pretty printer for graphs.
 */
object GraphPrettyPrinter {
  def toString(node: Node[_, _, _], indent: String = ""): String = {
    val sb = new StringBuilder(indent + "|__" + node.conf)
    if (node.base.isDefined) {
      sb.append("*******")
    }
    for (edge <- node.outs) {
      sb.append("\n  " + indent + "|" + (if (edge.driveInfo != null) edge.driveInfo else ""))
      sb.append("\n" + toString(edge.node, indent + "  "))
    }
    sb.toString
  }
}

/*! The simple lexicographic order on paths.
 */
object PathOrdering extends Ordering[Path] {
  @tailrec
  final def compare(p1: Path, p2: Path) =
    if (p1.length < p2.length) {
      -1
    } else if (p1.length > p2.length) {
      +1
    } else {
      val result = p1.head compare p2.head
      if (result == 0) {
        compare(p1.tail, p2.tail)
      } else {
        result
      }
    }
}