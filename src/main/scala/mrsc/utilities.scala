package mrsc

// used to store the old `in` edge
case class Tmp[C, D, E](node: Node[C, D, E], in: In[C, D, E])

object Transformations {
  def transpose[C, D, E](coGraph: CoGraph[C, D, E]): Graph[C, D, E] = {
    val leafPathes = coGraph.leaves.map(_.coPath)
    val levels = coGraph.nodes.groupBy(_.coPath.length).toList.sortBy(_._1).map(_._2)
    val (tNodes, tLeaves) = subTranspose(levels, leafPathes)
    val nodes = tNodes map { _.node }
    val leaves = tLeaves map { _.node }
    return Graph(nodes(0), leaves)
  }

  // sub-transposes cogpaph into graph level-by-level
  private def subTranspose[C, D, E](nodes: List[CoNodes[C, D, E]], leaves: List[Path]): (List[Tmp[C, D, E]], List[Tmp[C, D, E]]) =
    nodes match {
      case Nil =>
        (Nil, Nil)

      // leaves only??
      case ns1 :: Nil =>
        val tmpNodes: List[Tmp[C, D, E]] = ns1 map { n =>
          val node = Node[C, D, E](label = n.label, extra = n.info, outs = Nil, base = n.base, path = n.path)
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
          val edges = children map { tmp => Edge(tmp.node, tmp.in.label) }
          val node = new Node(n.label, n.info, edges, n.base, n.path)
          Tmp(node, n.in)
        }
        val tmpLeaves = tmpNodes.filter { tmp => leaves.contains(tmp.node.coPath) }
        (tmpNodes, tmpLeaves ++ leaves1)
      }
    }
}

object GraphPrettyPrinter {
  def toString(node: Node[_, _, _], indent: String = ""): String = {
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