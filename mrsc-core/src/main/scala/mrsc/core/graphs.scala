package mrsc.core

import scala.annotation.tailrec
/* # SC Graph Abstraction

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

 * `SGraph` (stack graph) - good for easy bottom-up traversals (a node knows about in) and
    for using in multi-result supercompilation
    (graph consists of complete and incomplete parts,
    and operation to add outs to incomplete nodes is cheap).
    This data structure is very similar to "spaghetti stack" - http://en.wikipedia.org/wiki/Spaghetti_stack
 * `TGraph` (tree graph) - good for top-down traversals (a node knows about its outs).

 The main idea here is to use functional (immutable) data structures in order to support
 multi-results composed of shared data.
 */

/** `SNode[C, D]` is dual to `TNode[C, D]`.
  */
case class SNode[C, D](conf: C, in: SEdge[C, D], base: Option[SPath], sPath: SPath) {

  lazy val tPath: List[Int] = sPath.reverse

  val ancestors: List[SNode[C, D]] =
    if (in == null) List() else in.node :: in.node.ancestors

  override def toString: String = conf.toString
}

case class SEdge[C, D](node: SNode[C, D], driveInfo: D)

/** `Graph[C, D, E]` is a core data structure in MRSC.
  * It may represent (1) a "work in progress" and (2) a completed graph.
  * We know already processed part of an SC graph
  * (`completeLeaves`, `completeNodes`) and a frontier
  * of incomplete part (`incompleteLeaves`).
  *
  * `C` (configuration) is a type of node label;
  * `D` (driving) is a type of edge label (driving info);
  */
case class SGraph[C, D](
    incompleteLeaves: List[SNode[C, D]],
    completeLeaves: List[SNode[C, D]],
    completeNodes: List[SNode[C, D]],
    prev: Option[SGraph[C, D]] = None,
) {

  val isComplete: Boolean = incompleteLeaves.isEmpty
  val current: SNode[C, D] = if (isComplete) null else incompleteLeaves.head
  lazy val size: Int = completeNodes.size + incompleteLeaves.size
  // current depth
  lazy val depth: Int = current.ancestors.size + 1
}

/*! `TNode[C, D, E]` is a very simple and straightforward implementation of
 * a top-down node.
 */
case class TNode[C, D](conf: C, outs: List[TEdge[C, D]], base: Option[TPath], tPath: TPath) {

  lazy val sPath: List[Int] = tPath.reverse

  @tailrec
  final def get(relTPath: TPath): TNode[C, D] = relTPath match {
    case Nil     => this
    case i :: rp => outs(i).node.get(rp)
  }
}

/*! The labeled directed edge. `node` is a destination node; `D` is driving info.
 */
case class TEdge[C, D](node: TNode[C, D], driveInfo: D)

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
case class TGraph[C, D](root: TNode[C, D], leaves: List[TNode[C, D]], focus: Option[TPath] = None) {
  def get(tPath: TPath): TNode[C, D] = root.get(tPath)
  override def toString: String = GraphPrettyPrinter.toString(this)
}

/*! Auxiliary data for transposing a graph into a transposed graph.
 */
case class Tmp[C, D](node: TNode[C, D], in: SEdge[C, D])

/*! A transformer of stack graphs into tree graphs.
 */
object Transformations {
  /*! Transposition is done in the following simple way. Nodes are grouped according to the
   levels (the root is 0-level). Then graphs are produced from in bottom-up fashion.
   */
  def transpose[C, D, E](g: SGraph[C, D]): TGraph[C, D] = {
    val allLeaves = g.completeLeaves ++ g.incompleteLeaves
    val allNodes = g.completeNodes ++ g.incompleteLeaves
    val orderedNodes = allNodes.sortBy(_.sPath)(PathOrdering)
    val rootNode = orderedNodes.head

    val leafPathes = allLeaves.map(_.sPath)
    val levels = orderedNodes.groupBy(_.sPath.length).toList.sortBy(_._1).map(_._2)
    val sortedLevels = levels.map(_.sortBy(_.tPath)(PathOrdering))
    val (tNodes, tLeaves) = subTranspose(sortedLevels, leafPathes)
    val nodes = tNodes map { _.node }
    val leaves = tLeaves map { _.node }
    TGraph(nodes.head, leaves, Option(g.current).map(_.tPath))
  }

  // sub-transposes graph into transposed graph level-by-level
  private def subTranspose[C, D](
      nodes: List[List[SNode[C, D]]],
      leaves: List[TPath],
  ): (List[Tmp[C, D]], List[Tmp[C, D]]) =
    nodes match {
      case Nil =>
        (Nil, Nil)

      // leaves only??
      case ns1 :: Nil =>
        val tmpNodes: List[Tmp[C, D]] = ns1 map { n =>
          val node = TNode[C, D](n.conf, Nil, n.base.map(_.reverse), n.tPath)
          Tmp(node, n.in)
        }
        val tmpLeaves = tmpNodes.filter { tmp =>
          leaves.contains(tmp.node.sPath)
        }
        (tmpNodes, tmpLeaves)

      case ns1 :: ns =>
        val (allCh, leaves1) = subTranspose(ns, leaves)
        val allchildren = allCh.groupBy { _.node.sPath.tail }
        val tmpNodes = ns1 map { n =>
          val children: List[Tmp[C, D]] = allchildren.getOrElse(n.sPath, Nil)
          val edges = children map { tmp => TEdge(tmp.node, tmp.in.driveInfo) }
          val node = TNode(n.conf, edges, n.base.map(_.reverse), n.tPath)
          Tmp(node, n.in)
        }
        val tmpLeaves = tmpNodes.filter { tmp => leaves.contains(tmp.node.sPath) }
        (tmpNodes, tmpLeaves ++ leaves1)
    }
}

/*! Ad Hoc console pretty printer for graphs.
 */
object GraphPrettyPrinter {
  def toString(tg: TGraph[_, _]): String = {
    val focus = tg.focus
    toString(tg.root, focus)
  }

  def toString(node: TNode[_, _], focus: Option[TPath], indent: String = ""): String = {
    val sb = new StringBuilder()

    sb.append(indent + "|__" + node.conf)
    if (node.base.isDefined)
      sb.append("*")
    if (focus.contains(node.tPath))
      sb.append(" <===")
    for (edge <- node.outs) {
      sb.append("\n  " + indent + "|" + (if (edge.driveInfo != null) edge.driveInfo else ""))
      sb.append("\n" + toString(edge.node, focus, indent + "  "))
    }
    sb.toString
  }
}

/*! The simple lexicographic order on paths.
 */
object PathOrdering extends Ordering[TPath] {
  @tailrec
  final def compare(p1: TPath, p2: TPath): Int =
    if (p1.length < p2.length) -1
    else if (p1.length > p2.length) +1
    else {
      val result = p1.head compare p2.head
      if (result == 0) {
        compare(p1.tail, p2.tail)
      } else {
        result
      }
    }
}
