package mrsc.pfp.samples

import mrsc.core._
import mrsc.pfp._

case class SC1(val gc: GContext) extends PFPRules
  with PFPSemantics
  with PositiveDriving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3ByCouplingWhistle
  with UpperMsgOrLowerMggOnBinaryWhistle
  
object SC1 extends SC

case class SC2(val gc: GContext) extends PFPRules
  with PFPSemantics
  with PositiveDriving
  with AllFoldingCandidates
  with Folding
  with ControlEmbeddingCandidates
  with HE3ByCouplingWhistle
  with LowerMsgOrUpperMsgOnBinaryWhistle
  
object SC2 extends SC

case class SC3(val gc: GContext) extends PFPRules
  with PFPSemantics
  with PositiveDriving
  with AllFoldingCandidates
  with Folding
  with ControlEmbeddingCandidates
  with HE3ByCouplingWhistle
  with UpperMsgOrLowerMsgOnBinaryWhistle
  
object SC3 extends SC

