package mrsc.pfp.experiments

import mrsc.pfp._

object EmbeddingsDetectors {
  case class SC1(val gc: GContext) extends PFPRules
    with PFPSemantics
    with PositiveDriving
    with AllEmbeddingCandidates
    with HE3ByCouplingWhistle
    with FoldingOnBinaryWhistle

  object SC1 extends PFPSC

  case class SC2(val gc: GContext) extends PFPRules
    with PFPSemantics
    with PositiveDriving
    with ControlEmbeddingCandidates
    with HE3ByCouplingWhistle
    with FoldingOnBinaryWhistle

  object SC2 extends PFPSC
}
