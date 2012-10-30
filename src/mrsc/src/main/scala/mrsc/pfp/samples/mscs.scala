package mrsc.pfp.samples

import mrsc.core._
import mrsc.pfp._

case class AllMSC(val gc: GContext) extends PFPRules
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3Whistle
  with AllRebuildings

object AllMSC extends SC

// All rebuildings but only on whistle
case class MSC1(val gc: GContext) extends PFPRules
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3Whistle
  with LowerRebuildingsOnBinaryWhistle

object MSC1 extends SC

case class MSC2(val gc: GContext) extends PFPRules
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3Whistle
  with LowerAllBinaryGensOnBinaryWhistle

object MSC2 extends SC

case class MSC3(val gc: GContext) extends PFPRules
  //with PFPSyntax
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3ByCouplingWhistle
  with LowerAllBinaryGensOnBinaryWhistle

object MSC3 extends SC

case class MSC4(val gc: GContext) extends PFPRules
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3ByCouplingWhistle
  with UpperAllBinaryGensOnBinaryWhistle

object MSC4 extends SC

case class MSC5(val gc: GContext) extends PFPRules
  with SizedRebuildingsGenerator
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3Whistle
  with AllRebuildings {

  val genSize = 1
}

object MSC5 extends SC

case class MSC6(val gc: GContext) extends PFPRules
  with SizedRebuildingsGenerator
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3ByCouplingWhistle
  with UpperAllBinaryGensOnBinaryWhistle {
  val genSize = 2
}


object MSC6 extends SC

case class AllMSC1(val gc: GContext) extends PFPRules
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3Whistle
  with AllRebuildings
  with SizeGraphFilter {
  val maxGraphSize = 20
}

object AllMSC1 extends SC

