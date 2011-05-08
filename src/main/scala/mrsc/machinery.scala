package mrsc

/*!# Whistle and tricks
  
 `GenericMultiMachine` is well suited for implementing different aspects in traits.
  
 It turns out that "pure" multi-result supercompilation is limited by whistle only. 
 "Advanced" (such as two-level) multi-result supercompilation is limited by additional tricks 
 (such as improvement lemma) applied during supercompilation. 
  
 So `W` here stands for "whistle signal".
*/
trait GenericMultiMachine[C, D, E, W] extends MultiResultMachine[C, D, E] {

  def blame(pState: PState[C, D, E]): W
  def fold(pState: PState[C, D, E]): List[Path]
  def drive(whistle: W, pState: PState[C, D, E]): List[MStep[C, D, E]]
  def rebuildings(whistle: W, pState: PState[C, D, E]): List[MStep[C, D, E]]
  def tricks(whistle: W, pState: PState[C, D, E]): List[MStep[C, D, E]]

  /*! The logic of this machine is straightforward:
     
     * If there are opportunities for folding, lets fold.
     
     * Otherwise, lets try all variants.
    
   Note that the whistle signal is passed to `drive`, `rebuildings` and `tricks`.
  */
  override def makeSteps(pState: PState[C, D, E]): List[MStep[C, D, E]] =
    fold(pState) match {

      case foldPaths if !foldPaths.isEmpty =>
        foldPaths map MFold

      case _ =>
        val signal = blame(pState)
        val driveSteps = drive(signal, pState)
        val genSteps = rebuildings(signal, pState)
        val trickySteps = tricks(signal, pState)
        driveSteps ++ (trickySteps ++ genSteps)

    }
}