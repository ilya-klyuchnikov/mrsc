package mrsc

/*!# Modeling API
 */
sealed trait Step[+C, +D, +E]

case object MComplete extends Step[Nothing, Nothing, Nothing]
// prune the current tree
case object MPrune extends Step[Nothing, Nothing, Nothing]
// add outs to the current node
case class MForest[C, D, E](val subSteps: List[SubStep[C, D, E]]) extends Step[C, D, E]
// replace the current node - "generalization" of current expression
// E is an extra information
case class MReplace[C, D, E](val label: C, val extra: E) extends Step[C, D, E]
// rollback to the dangerous node and replaces it with new label -
// generalization of dangerous expression
case class MRollback[C, D, E](val dangerous: CoNode[C, D, E], val safe: C, val extra: E) extends Step[C, D, E]
// folding
case class MFold(val path: Path) extends Step[Nothing, Nothing, Nothing]

/* Language-agnostic modeling of abstract computations */
case class SubStep[+C, +D, +E](label: C, info: D, extra: E)

trait SingleMachine[C, D, E] {
  def makeStep(pState: PState[C, D, E]): Step[C, D, E]
}

trait MultiMachine[C, D, E] {
  def makeSteps(pState: PState[C, D, E]): List[Step[C, D, E]]
}

// None for pruned graphs - just for information
// Some(g) - for complete graph
trait CoGraphConsumer[C, D, E] {
  val description: String
  def consume(graph: Option[CoGraph[C, D, E]]): Unit
}

// just to distinguish modeling inconsistencies
// (the main one is "too much results")
class ModelingError(val message: String) extends Exception(message: String)