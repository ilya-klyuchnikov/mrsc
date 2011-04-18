package mrsc

/* Language-agnostic modeling of abstract computations */
class SubStep[C, I](val configuration: C, val info: I)

// the step is performed on "active node" 
abstract sealed class MStep[+C, +I]
// mark the active node as a leaf
case object MComplete extends MStep[Nothing, Nothing]
// prune the current tree
case object MPrune extends MStep[Nothing, Nothing]
// add children to the current node
case class MForest[C, I](val subSteps: List[SubStep[C, I]]) extends MStep[C, I]
// replace the current node
case class MReplace[C, I](val configuration: C, val info: I) extends MStep[C, I]
// folding
case class MFold(val path: Path) extends MStep[Nothing, Nothing]

trait SingleMachine[C, I] {
  def makeStep(pState: PState[C, I]): MStep[C, I]
}

trait MultiMachine[C, I] {
  def makeSteps(pState: PState[C, I]): List[MStep[C, I]]
}

// None for pruned graphs - just for information
// Some(g) - for complete graph
trait CoGraphConsumer[C, I] {
  val description: String
  def consume(graph: Option[CoGraph[C, I]]): Unit
}

// just to distinguish modeling inconcistencies 
// (the main one is "to much results")
class ModelingError(val message: String) extends Exception(message: String)