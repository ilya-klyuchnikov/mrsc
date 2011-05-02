package mrsc

import scala.annotation.tailrec
/*! # SCP Graph Abstraction
 
 At the heart of MRSC is a mini-framework for manipulating SCP graphs.
 In MRSC SC Graph is a special kind of graph that can be seen as a tree (skeleton), some leaves of which can have loopbacks.
 Note that loopbacks can start only from leaves. 
 
 Here is the simplest SCP graph. There are two loopbacks: j⇢c and h⇢d.

         . c
        .  ↓
      .    d
     .     ↓  .  
    .      e   . 
    .   ↙ ↓ ↘ .  
    .  f   g   h
     . ↓
       j
       
 SCP graphs in MRSC are used in three forms:
 
 * `Graph` - good for top-down traversals.
 * `CoGraph` - good for easy bottom-up traversals.
 * `PartialCoGraph` - good for using in multi-result supercompilation.
 
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
    if (base.isDefined) {
      sb.append("*******")
    }
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
case class CoNode[+C, +I](configuration: C, info: I, parent: CoNode[C, I], base: Option[Path], coPath: CoPath) {
  lazy val path = coPath.reverse
  val ancestors: List[CoNode[C, I]] = if (parent == null) Nil else parent :: parent.ancestors
  override def toString = configuration.toString
}

case class PState[+C, +I](val node: CoNode[C, I], val completeNodes: List[CoNode[C, I]])

case class PartialCoGraph[C, I](
  completeLeaves: List[CoNode[C, I]],
  incompleteLeaves: List[CoNode[C, I]],
  completeNodes: List[CoNode[C, I]]) { // accumulator of nodes

  val activeLeaf: Option[CoNode[C, I]] = incompleteLeaves.headOption
  // "partial state" of this partial co-graph
  val pState = PState(activeLeaf.getOrElse(null), completeNodes)

  // step is suggested for the current active leaf
  def addStep(step: Step[C, I]): PartialCoGraph[C, I] = {
    //println(step)
    incompleteLeaves match {
      case aLeaf :: ls =>
        step match {
          case MComplete =>
            new PartialCoGraph(aLeaf :: completeLeaves, ls, aLeaf :: completeNodes)
          case MPrune =>
            throw new Error()
          case MReplace(c, _) =>
            val newNode = CoNode(c, aLeaf.info, aLeaf.parent, None, aLeaf.coPath)
            new PartialCoGraph(completeLeaves, newNode :: ls, completeNodes)
          case MRollback(dangNode, c, _) =>
            val newNode = CoNode(c, dangNode.info, dangNode.parent, None, dangNode.coPath)
            val newCompleteNodes = completeNodes.remove(n => n.path.startsWith(dangNode.path))
            val newCompleteLeaves = completeLeaves.remove(n => n.path.startsWith(dangNode.path))
            val newIncompleteLeaves = ls.remove( n => n.path.startsWith(dangNode.path))
            val newg = new PartialCoGraph(newCompleteLeaves, newNode :: newIncompleteLeaves, newCompleteNodes)
            newg
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