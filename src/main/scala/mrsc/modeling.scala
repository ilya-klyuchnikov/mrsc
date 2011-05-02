package mrsc

/*!# Modeling API
 */
sealed trait Step[+C, +I]

case object MComplete extends Step[Nothing, Nothing]
// prune the current tree
case object MPrune extends Step[Nothing, Nothing]
// add children to the current node
case class MForest[C, I](val subSteps: List[SubStep[C, I]]) extends Step[C, I]
// replace the current node - "generalization" of current expression
case class MReplace[C, I](val configuration: C, val info: I) extends Step[C, I]
// rollback to the dangerous node and replaces it with new configuration -
// generalization of dangerous expression
case class MRollback[C, I](val dangerous: CoNode[C, I], val safe: C, val info: I) extends Step[C, I]
// folding
case class MFold(val path: Path) extends Step[Nothing, Nothing]

/* Language-agnostic modeling of abstract computations */
case class SubStep[+C, +I](configuration: C, info: I)

trait SingleMachine[C, I] {
  def makeStep(pState: PState[C, I]): Step[C, I]
}

trait MultiMachine[C, I] {
  def makeSteps(pState: PState[C, I]): List[Step[C, I]]
}

// None for pruned graphs - just for information
// Some(g) - for complete graph
trait CoGraphConsumer[C, I] {
  val description: String
  def consume(graph: Option[CoGraph[C, I]]): Unit
}

// just to distinguish modeling inconsistencies 
// (the main one is "to much results")
class ModelingError(val message: String) extends Exception(message: String)