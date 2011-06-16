package mrsc

case class Contraction[C](v: Name, pat: C) {
  override def toString = v + " = " + pat
}

abstract sealed trait DriveStep[+C]
case class TransientDriveStep[C](next: C) extends DriveStep[C]
case object StopDriveStep extends DriveStep[Nothing]
case class DecomposeDriveStep[C](compose: List[C] => C, parts: List[C]) extends DriveStep[C]
case class VariantsDriveStep[C](cases: List[(Contraction[C], C)]) extends DriveStep[C]

abstract sealed class DriveInfo[+C]
case object TransientStepInfo extends DriveInfo[Nothing] {
  override def toString = "->"
}
case class DecomposeStepInfo[C](compose: List[C] => C) extends DriveInfo[C] {
  override def toString = ""
}
case class VariantsStepInfo[C](contr: Contraction[C]) extends DriveInfo[C] {
  override def toString = contr.toString
}

sealed trait Extra
case object NoExtra extends Extra

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
    drive(pState.node.conf) match {
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
    !isDrivable(pState.node.conf)
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