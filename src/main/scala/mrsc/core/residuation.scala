package mrsc.core

import scala.annotation.tailrec

/*! The labeled directed edge. `N` is a destination node; `D` is driving info.
 */
case class TEdge[C, D, E](tNode: TNode[C, D, E], driveInfo: D)

/*! `TGraph[C, D, E]`.
 * `TGraph` is a representation of the graph of configurations
 * that is good for top-down traversals (a node knows about its outs).
 * 
 * A `TGraph` is a "transposed" representation of a `Graph`,
 * each Node being replaced with TNode,
 * and each Edge being replaced with TEdge.
 *`C` (configuration) is a type of node label; 
 *`D` (driving) is a type of edge label (driving info);
 *`E` (extra information) is a type of extra label of a node (extra info). 
 * Extra information may be seen as an additional "instrumentation" of SC graph.
 */
case class TGraph[C, D, E](root: TNode[C, D, E], leaves: List[TNode[C, D, E]]) {
  def get(tPath: TPath): TNode[C, D, E] = root.get(tPath)
  override def toString = root.toString
}

/*! `TNode[C, D, E]` is a very simple and straightforward implementation of
 * a top-down node. 
 */
case class TNode[C, D, E](
  conf: C,
  extraInfo: E,
  outs: List[TEdge[C, D, E]],
  back: Option[TPath],
  tPath: TPath) {

  lazy val path = tPath.reverse

  @tailrec
  final def get(relTPath: TPath): TNode[C, D, E] = relTPath match {
    case Nil => this
    case i :: rp => outs(i).tNode.get(rp)
  }

  val isLeaf = outs.isEmpty
  val isRepeat = back.isDefined
  
  override def toString = GraphPrettyPrinter.toString(this)
}

/*! Auxiliary data for transposing a graph into a transposed graph.
 */
case class Tmp[C, D, E](node: TNode[C, D, E], in: Edge[C, D, E])

/*! A transformer of graphs into transposed graphs.
 */
object Transformations {
  /*! Transposition is done in the following simple way. Nodes are grouped according to the 
   levels (the root is 0-level). Then graphs are produced from in bottom-up fashion.
   */
  def transpose[C, D, E](g: Graph[C, D, E]): TGraph[C, D, E] = {
    require(g.isComplete)
    val allLeaves = g.incompleteLeaves ++ g.completeLeaves
    val allNodes = g.incompleteLeaves ++ g.completeNodes
    val orderedNodes = allNodes.sortBy(_.path)(PathOrdering)
    val rootNode = orderedNodes.head

    val leafPathes = allLeaves.map(_.path)
    val levels = orderedNodes.groupBy(_.path.length).toList.sortBy(_._1).map(_._2)
    val sortedLevels = levels.map(_.sortBy(_.tPath)(PathOrdering))
    val (tNodes, tLeaves) = subTranspose(sortedLevels, leafPathes)
    val nodes = tNodes map { _.node }
    val leaves = tLeaves map { _.node }
    return TGraph(nodes(0), leaves)
  }

  // sub-transposes graph into transposed graph level-by-level
  private def subTranspose[C, D, E](
    nodes: List[List[Node[C, D, E]]],
    leaves: List[TPath]): (List[Tmp[C, D, E]], List[Tmp[C, D, E]]) =
    nodes match {
      case Nil =>
        (Nil, Nil)

      // leaves only??
      case ns1 :: Nil =>
        val tmpNodes: List[Tmp[C, D, E]] = ns1 map { n =>
          val node = TNode[C, D, E](n.conf, n.extraInfo, Nil, n.back.map(_.reverse), n.tPath)
          Tmp(node, n.in)
        }
        val tmpLeaves = tmpNodes.filter { tmp =>
          leaves.contains(tmp.node.path)
        }
        (tmpNodes, tmpLeaves)

      case ns1 :: ns => {
        val (allCh, leaves1) = subTranspose(ns, leaves)
        val allchildren = allCh.groupBy { _.node.path.tail }
        val tmpNodes = ns1 map { n =>
          val children: List[Tmp[C, D, E]] = allchildren.getOrElse(n.path, Nil)
          val edges = children map { tmp => TEdge(tmp.node, tmp.in.driveInfo) }
          val node = new TNode(n.conf, n.extraInfo, edges, n.back.map(_.reverse), n.tPath)
          Tmp(node, n.in)
        }
        val tmpLeaves = tmpNodes.filter { tmp => leaves.contains(tmp.node.path) }
        (tmpNodes, tmpLeaves ++ leaves1)
      }
    }
}

/*! Ad Hoc console pretty printer for graphs.
 */
object GraphPrettyPrinter {
  def toString(node: TNode[_, _, _], indent: String = ""): String = {
    val sb = new StringBuilder(indent + "|__" + node.conf)
    if (node.back.isDefined) {
      sb.append("*******")
    }
    for (edge <- node.outs) {
      sb.append("\n  " + indent + "|" + (if (edge.driveInfo != null) edge.driveInfo else ""))
      sb.append("\n" + toString(edge.tNode, indent + "  "))
    }
    sb.toString
  }
}

/*! The simple lexicographic order on paths.
 */
object PathOrdering extends Ordering[TPath] {
  @tailrec
  final def compare(p1: TPath, p2: TPath) =
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
