package mrsc.pfp

import mrsc.core._
import mrsc.core.GraphRewriteStep._
import scalaz.Show

// classical deforestation transformation
case class Deforester(gc: GContext)
    extends PFPRules
    with PFPSemantics
    with Driving
    with AllFoldingCandidates
    with Folding
    with NoWhistle
    with NoRebuildings

// A supercompiler driven by a user.
// (User decides which way to go).
case class InteractiveMRSC(gc: GContext)
    extends PFPRules
    with PFPSemantics
    with Driving
    with AllFoldingCandidates
    with Folding
    with AllEmbeddingCandidates
    with NoWhistle
    with AllRebuildings {

  val prettyPrinter: PFPGraphPrettyPrinter = new PFPGraphPrettyPrinter {
    implicit def termShow[T <: MetaTerm]: Show[T] = NamelessShows.TermShow
  }

  var userSteps: List[Int] = Nil

  override def steps(g: G): List[S] = {
    val allSteps = super.steps(g)
    println()
    val tGraph = Transformations.transpose(g)
    println(prettyPrinter.toString(tGraph))

    for { (step, i) <- allSteps.zipWithIndex } {
      Console.println(s"[$i] ${showStep(step)}")
    }
    val idx = scala.io.StdIn.readInt()
    userSteps = userSteps :+ idx
    allSteps(idx) :: Nil
  }

  def showStep(step: S): String = step match {
    case RebuildStep(c)            => "RB: " + NamelessShows.s(c)
    case AddChildNodesStep(cs)     => "CH: " + cs.map(x => NamelessShows.s(x._1)).mkString(", ")
    case FoldStep(_)               => "FOLD"
    case CompleteCurrentNodeStep() => "Complete"
  }
}

// Accepts steps from InteractiveMRSC.
case class DebugMRSC(gc: GContext, userSteps: List[Int])
    extends PFPRules
    with PFPSemantics
    with Driving
    with AllFoldingCandidates
    with Folding
    with AllEmbeddingCandidates
    with NoWhistle
    with AllRebuildings {

  val prettyPrinter: PFPGraphPrettyPrinter = new PFPGraphPrettyPrinter {
    implicit def termShow[T <: MetaTerm]: Show[T] = NamelessShows.TermShow
  }

  var i = 0

  override def steps(g: G): List[S] = {
    val step = super.steps(g)(userSteps(i))
    i = i + 1
    step :: Nil
  }
}

// Supercompiler considering all variants
// The depth of the graph is bound.
case class DepthBoundMRSC(gc: GContext, maxDepth: Int)
    extends PFPRules
    with PFPSemantics
    with Driving
    with AllFoldingCandidates
    with Folding
    with AllEmbeddingCandidates
    with NoWhistle
    with AllRebuildings
    with DepthGraphFilter

case class SC1(gc: GContext)
    extends PFPRules
    with PFPSemantics
    with PositiveDriving
    with AllFoldingCandidates
    with Folding
    with AllEmbeddingCandidates
    with HE3ByCouplingWhistle
    with UpperMsgOrLowerMggOnBinaryWhistle

object SC1 extends PFPSC

case class SC2(gc: GContext)
    extends PFPRules
    with PFPSemantics
    with PositiveDriving
    with AllFoldingCandidates
    with Folding
    with ControlEmbeddingCandidates
    with HE3ByCouplingWhistle
    with LowerMsgOrUpperMsgOnBinaryWhistle

object SC2 extends PFPSC

case class SC3(gc: GContext)
    extends PFPRules
    with PFPSemantics
    with PositiveDriving
    with AllFoldingCandidates
    with Folding
    with ControlEmbeddingCandidates
    with HE3ByCouplingWhistle
    with UpperMsgOrLowerMsgOnBinaryWhistle

object SC3 extends PFPSC
