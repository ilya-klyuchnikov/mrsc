package mrsc

/*!# Abstract steps
  
 Under the hood an abstract machine deals with some kind of semantics of the language.
 Low-level operations should be translated into high-level abstract operations (or messages) 
 over SC graphs.
 */
sealed trait Command[+C, +D, +E]

/*! The step `MMakeLeaf` means that the current branch of the graph in focus is
   a terminal node (leaf).
 */
case object ConvertToLeaf extends Command[Nothing, Nothing, Nothing]

/*! `MAddForest` corresponds to development of current branch of the graph (driving in 90%).
 Development is divided into several `subSteps`.
 */
case class AddChildNodes[+C, +D, +E](val childNodes: List[ChildNode[C, D, E]]) extends Command[C, D, E]

/* Usually `SubStep` hides internals of driving.  */
case class ChildNode[+C, +D, +E](conf: C, driveInfo: D, extraInfo: E)

/*! `MFold` signals that there is a path to something similar to the current state in the past
 of the current SC Graph.
 */
case class MakeFold(val path: Path) extends Command[Nothing, Nothing, Nothing]

/*! The step `MPrune` means that the current graph should be discarded. In the case of 
 the "single-result" supercompilation it means failure (= no good result). In the case of
 multi-result supercompilation it means that we should continue with the next variant.   
 */
case object DiscardGraph extends Command[Nothing, Nothing, Nothing]

/*! `MReplace` is fixing the current state of the branch.
 */
case class ReplaceNode[C, D, E](val conf: C, val extraInfo: E) extends Command[C, D, E]

/*! `MRollback` is fixing the past `dangerous` state of the current branch.
 */
case class RollbackSubGraph[C, D, E](val subGraphRoot: CoNode[C, D, E],
  val conf: C, val extraInfo: E) extends Command[C, D, E]

/*!# Modeling expectations
 */

/*! The following exception usually means that some modeling expectation (or hypothesis) 
 was not met during actual modeling.  
 */
class ModelingError(val message: String) extends Exception(message: String)

/*!# Whistle and tricks
  
 `GenericMultiMachine` is well suited for implementing different aspects in traits.
  
 It turns out that "pure" multi-result supercompilation is limited by whistle only. 
 "Advanced" (such as two-level) multi-result supercompilation is limited by additional tricks 
 (such as improvement lemma) applied during supercompilation. 
  
 So `W` here stands for "whistle signal".
*/
trait GenericMultiMachine[C, D, E] extends Machine[C, D, E] {

  type W = Option[CoNode[C, D, E]]
  def isLeaf(pState: PState[C, D, E]): Boolean
  def fold(pState: PState[C, D, E]): Option[Path]
  def blame(pState: PState[C, D, E]): W
  def drive(whistle: W, pState: PState[C, D, E]): List[Command[C, D, E]]
  def rebuildings(whistle: W, pState: PState[C, D, E]): List[Command[C, D, E]]
  def tricks(whistle: W, pState: PState[C, D, E]): List[Command[C, D, E]]

  /*! The logic of this machine is straightforward:
     
     * If there are opportunities for folding, lets fold.
     
     * Otherwise, lets try all variants.
    
   Note that the whistle signal is passed to `drive`, `rebuildings` and `tricks`.
  */
  override def steps(pState: PState[C, D, E]): List[Command[C, D, E]] =
    if (isLeaf(pState))
      List(ConvertToLeaf)
    else fold(pState) match {
      case Some(path) =>
        List(MakeFold(path))
      case _ =>
        val signal = blame(pState)
        val driveSteps = drive(signal, pState)
        val genSteps = rebuildings(signal, pState)
        val trickySteps = tricks(signal, pState)
        driveSteps ++ (trickySteps ++ genSteps)
    }
}

// doesn't not care about whistle signals 
trait Driving[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with Semantics[C] {
  override def drive(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] =
    eval(pState.node.conf) match {
      case StopDriveStep =>
        List()
      case DecomposeDriveStep(compose, args) =>
        val stepInfo = DecomposeStepInfo(compose)
        val subSteps = args map { a => ChildNode(a, stepInfo, NoExtra) }
        List(AddChildNodes(subSteps))
      case TransientDriveStep(next) =>
        val subSteps = List(ChildNode(next, TransientStepInfo, NoExtra))
        List(AddChildNodes(subSteps))
      case VariantsDriveStep(cases) =>
        val subSteps = cases map { case (contr, next) => ChildNode(next, VariantsStepInfo(contr), NoExtra) }
        List(AddChildNodes(subSteps))
    }

  override def isLeaf(pState: PState[C, DriveInfo[C], Extra]) =
    !isReducible(pState.node.conf)
}

trait SimpleDriving[C] extends Driving[C] {
  override def drive(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] =
    whistle match {
      case Some(blamed) => List()
      case None => super.drive(whistle, pState)
    }
}

trait PruningDriving[C] extends Driving[C] {
  override def drive(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] =
    whistle match {
      case Some(blamed) => List(DiscardGraph)
      case None => super.drive(whistle, pState)
    }
}