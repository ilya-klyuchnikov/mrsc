package mrsc.pfp.samples

import mrsc.core._
import mrsc.pfp._

case class ClassicSC(val gc: GContext) extends PFPRules
  with PFPSyntax
  with PFPSemantics
  with PositiveDriving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HEByCouplingWhistle
  with UpperMsgOrLowerMggOnBinaryWhistle
  
object ClassicSC extends SC
