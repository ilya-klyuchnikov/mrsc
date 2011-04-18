package mrsc

import scala.annotation.tailrec
/*
 * This graph is suitable for top-down traversion and/or modification
 */
case class Graph[C, I](root: Node[C, I], leaves: List[Node[C, I]]) {
  def get(path: Path): Node[C, I] = {
    root.get(path)
  }
  override def toString = root.toString
}

case class Node[C, I](configuration: C, info: I, children: List[Node[C, I]], base: Option[Path], path: Path) {
  lazy val coPath = path.reverse

  @tailrec
  final def get(sp: Path): Node[C, I] = {
    sp match {
      case Nil => this
      case i :: sp1 => children(i).get(sp1)
    }
  }

  def toString(indent: String): String = {
    val sb = new StringBuilder(indent + "|__" + configuration)
    for (edge <- children) {
      sb.append("\n  " + indent + "|" + (if (edge.info != null) edge.info else ""))
      sb.append("\n" + edge.toString(indent + "  "))
    }
    sb.toString
  }

  override def toString = toString("")
}

case class CoGraph[C, I](root: CoNode[C, I], leaves: List[CoNode[C, I]], nodes: List[CoNode[C, I]]) {
  override def toString = nodes.toString
}
case class CoNode[C, I](configuration: C, info: I, parent: CoNode[C, I], base: Option[Path], coPath: CoPath) {
  lazy val path = coPath.reverse
  val ancestors: List[CoNode[C, I]] = if (parent == null) Nil else parent :: parent.ancestors
  override def toString = configuration.toString
}

case class PState[C, I](val node: CoNode[C, I], val completeNodes: List[CoNode[C, I]])

case class PartialCoGraph[C, I](
  completeLeaves: List[CoNode[C, I]],
  incompleteLeaves: List[CoNode[C, I]],
  completeNodes: List[CoNode[C, I]]) { // accumulator of nodes

  val activeLeaf: Option[CoNode[C, I]] = incompleteLeaves.headOption
  // "partial state" of this partial co-graph
  val pState = PState(activeLeaf.getOrElse(null), completeNodes)

  // step is suggested for the current active leaf
  def addStep(step: MStep[C, I]): PartialCoGraph[C, I] = incompleteLeaves match {
    case aLeaf :: ls =>
      step match {
        case MComplete =>
          new PartialCoGraph(aLeaf :: completeLeaves, ls, aLeaf :: completeNodes)
        case MPrune =>
          throw new Error()
        case MReplace(c, i) =>
          val newNode = CoNode(c, i, aLeaf.parent, None, aLeaf.coPath)
          new PartialCoGraph(completeLeaves, newNode :: ls, completeNodes)
        case MForest(subSteps) =>
          val deltaLeaves = subSteps.zipWithIndex map {
            case (subStep, i) => CoNode(subStep.configuration, subStep.info, aLeaf, None, i :: aLeaf.coPath)
          }
          new PartialCoGraph(completeLeaves, deltaLeaves ++ ls, aLeaf :: completeNodes)
        case MFold(basePath) =>
          val l1 = CoNode(aLeaf.configuration, aLeaf.info, aLeaf.parent, Some(basePath), aLeaf.coPath)
          new PartialCoGraph(l1 :: completeLeaves, ls, l1 :: completeNodes)
      }
    case _ =>
      throw new Error()
  }
}

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