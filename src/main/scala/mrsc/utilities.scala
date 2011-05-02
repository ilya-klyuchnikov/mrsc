package mrsc

object Transformations {
  def transpose[C, I](coGraph: CoGraph[C, I]): Graph[C, I] = {
    val leafPathes = coGraph.leaves.map(_.coPath)
    val levels = coGraph.nodes.groupBy(_.coPath.length).toList.sortBy(_._1).map(_._2)
    val (nodes, leaves) = subTranspose(levels, leafPathes)
    return Graph(nodes(0), leaves)
  }

  // sub-transposes cogpaph into graph level-by-level
  private def subTranspose[C, I](nodes: List[List[CoNode[C, I]]], leaves: List[Path]): (List[Node[C, I]], List[Node[C, I]]) =
    nodes match {
      case Nil =>
        (Nil, Nil)

      case ns1 :: Nil =>
        val newNodes = ns1 map { n =>
          Node(label = n.label, info = n.info, outs = Nil, base = n.base, path = n.path)
        }
        val newLeaves = newNodes.filter { n =>
          leaves.contains(n.coPath)
        }
        (newNodes, newLeaves)

      case ns1 :: ns => {
        val (allCh, leaves1) = subTranspose(ns, leaves)
        val allchildren = allCh.groupBy { _.coPath.tail }
        val newNodes = ns1 map { n =>
          val children = allchildren.getOrElse(n.coPath, Nil)
          val edges = children map {n => Edge[Node[C, I], I](n, n.info)}
          new Node(n.label, n.info, edges, n.base, n.path)
        }
        val newLeaves = newNodes.filter { n => leaves.contains(n.coPath) }
        (newNodes, newLeaves ++ leaves1)
      }
    }
}

object GraphPrettyPrinter {
  def toString[C, I](node: Node[C, I], indent: String = ""): String = {
    val sb = new StringBuilder(indent + "|__" + node.label)
    if (node.base.isDefined) {
      sb.append("*******")
    }
    for (edge <- node.outs) {
      sb.append("\n  " + indent + "|" + (if (edge.label != null) edge.label else ""))
      sb.append("\n" + toString(edge.node, indent + "  "))
    }
    sb.toString
  }
}