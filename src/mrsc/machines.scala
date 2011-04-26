package mrsc

object Whistle extends Enumeration {
  type Whistle = Value
  val OK, SoftPrune, HardPrune, Complete = Value
}
/*
 * AbstractMultiMachine represents some common behavior logic.
 * It is more to help create "reference implementations", it is not for
 * good performance. It just a glue between different parts.
 * 
 * The base logic:
 * 1. Folding
 *    a) We can fold in different ways. Let's try all variants of folding.
 *    b) If we can fold, we do not try other steps 
 * 2. Whistle
 *    a) OK - everything is good
 *    b) SoftPrune - no driving, but generalization and tricks are possible
 *    c) HardPrune - delete this tree. Examples: a lemma was applied twice without result
 *    d) Complete - mark the current node as a complete one
 */

// TODO: there may be many drive case - propagate/not propagate info
trait BaseMultiMachine[C, I] extends MultiMachine[C, I] {
  override def makeSteps(pState: PState[C, I]): List[MStep[C, I]] = fold(pState) match {

    case foldPaths if !foldPaths.isEmpty =>
      foldPaths map { MFold(_) }

    case _ =>
      val whistle = terminate(pState)
      lazy val genSteps = rebuildings(pState, whistle).map(rebuildStep)
      lazy val trickySteps = tricks(pState, whistle).map(trickyStep)

      lazy val driveSteps = whistle match {
        case Whistle.OK => List(MForest(drive(pState)))
        case _ => List(MPrune)
      }

      whistle match {
        case Whistle.Complete => List(MComplete)
        case Whistle.HardPrune => driveSteps
        case _ => driveSteps ++ (trickySteps ++ genSteps)
      }
  }

  def fold(pState: PState[C, I]): List[Path]

  def terminate(pState: PState[C, I]): Whistle.Value

  def drive(pState: PState[C, I]): List[SubStep[C, I]]

  def rebuildings(pState: PState[C, I], signal: Whistle.Value): List[SubStep[C, I]]
  def rebuildStep(gs: SubStep[C, I]): MStep[C, I]

  def tricks(pState: PState[C, I], signal: Whistle.Value): List[SubStep[C, I]]
  def trickyStep(gs: SubStep[C, I]): MStep[C, I]
}