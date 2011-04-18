package mrsc

object Whistle extends Enumeration {
  type Whistle = Value
  val OK, SoftPrune, HardPrune = Value
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
 *    c) HardPrune - delete this tree
 */

trait BaseMultiMachine[C, I] extends MultiMachine[C, I] {
  override def makeSteps(pState: PState[C, I]): List[MStep[C, I]] = fold(pState) match {

    case foldPaths if !foldPaths.isEmpty =>
      foldPaths map { MFold(_) }

    case _ =>
      val whistle = prune_?(pState)
      lazy val genSteps = generalizations(pState, whistle).map(generalizationStep)
      lazy val trickySteps = tricks(pState, whistle).map(trickyStep)
      lazy val driveSteps = List(MForest(drive(pState)))
      lazy val pruneSteps = List(MPrune)

      prune_?(pState) match {
        case Whistle.HardPrune =>
          pruneSteps
        case Whistle.SoftPrune =>
          trickySteps ++ genSteps
        case Whistle.OK =>
          driveSteps ++ (trickySteps ++ genSteps)
      }
  }

  def fold(pState: PState[C, I]): List[Path]

  def prune_?(pState: PState[C, I]): Whistle.Value

  def drive(pState: PState[C, I]): List[SubStep[C, I]]

  def generalizations(pState: PState[C, I], signal: Whistle.Value): List[SubStep[C, I]]

  def tricks(pState: PState[C, I], signal: Whistle.Value): List[SubStep[C, I]]

  def generalizationStep(gs: SubStep[C, I]): MStep[C, I]

  def trickyStep(gs: SubStep[C, I]): MStep[C, I]
}