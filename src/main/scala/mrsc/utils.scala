package mrsc

object Transformations {
  def transpose[C, I](coGraph: CoGraph[C, I]): Graph[C, I] = {
    val leafPathes = coGraph.leaves.map(_.coPath)
    val levels = coGraph.nodes.groupBy(_.coPath.length).toList.sortBy(_._1).map(_._2)
    val (nodes, leaves) = subTranspose(levels, leafPathes)
    return Graph(nodes(0), leaves)
  }

  private def subTranspose[C, I](nodes: List[List[CoNode[C, I]]], leaves: List[Path]): (List[Node[C, I]], List[Node[C, I]]) =
    nodes match {
      case Nil => (Nil, Nil)
      case ns1 :: Nil =>
        val newNodes = ns1 map { n => new Node(n.configuration, n.info, Nil, n.base, n.path) }
        val newLeaves = newNodes.filter { n => leaves.contains(n.coPath) }
        (newNodes, newLeaves)
      case ns1 :: ns => {
        val (allCh, leaves1) = subTranspose(ns, leaves)
        val allchildren = allCh.groupBy { _.coPath.tail }
        val newNodes = ns1 map { n =>
          val children = allchildren.getOrElse(n.coPath, Nil)
          new Node(n.configuration, n.info, children, n.base, n.path)
        }
        val newLeaves = newNodes.filter { n => leaves.contains(n.coPath) }
        (newNodes, newLeaves ++ leaves1)
      }
    }
}