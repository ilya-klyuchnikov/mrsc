package mrsc.pfp

// classical deforestation transformation
case class Deforester(val gc: GContext)
  extends PFPRules
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with NoWhistle
  with NoRebuildings
