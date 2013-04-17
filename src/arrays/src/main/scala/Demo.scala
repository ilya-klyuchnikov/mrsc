package mrsc.pfp.arrays

import scalaz._
import Scalaz._
import mrsc.core._
import mrsc.pfp._

trait WithErrorSemantics extends PFPSemantics {
  val ERROR = Ctr("ERROR", Nil)
  override def driveStep(t: MetaTerm): MStep =
    t match {
      case rb: Rebuilding =>
        super.driveStep(t)
      case t: Term => Decomposition.decompose(t) match {
        case context @ Context(RedexCaseCtr(`ERROR`, _)) =>
          TransientMStep(context.replaceHole(ERROR))
        case _ =>
          super.driveStep(t)
      }
    }
}

trait Demo {

  val bindings: GContext
  implicit def termShow[T <: MetaTerm]: Show[T] = NamelessShows.TermShow

  val prettyPrinter = new PFPGraphPrettyPrinter {
    implicit def termShow[T <: MetaTerm]: Show[T] = NamelessShows.TermShow
  }

  val mxUI = new PFPGraphUI {
    implicit def termShow[T <: MetaTerm]: Show[T] = NamelessShows.TermShow
  }

  def deforest(bindings: GContext, goal: Term) = {
    case class Deforester(val gc: GContext)
      extends PFPRules
      with WithErrorSemantics
      with Driving
      with AllFoldingCandidates
      with Folding
      with NoWhistle
      with NoRebuildings

    val rules = new Deforester(bindings)
    val g = GraphGenerator(rules, goal).toList.head
    val g1 = Transformations.transpose(g)
    println(prettyPrinter.toStringDense(g1))
    mxUI.showMxGraph(g1)
    Residuator(g1).result
  }

  def run(in: String) {
    val goal: Term = in
    println("   " + goal.shows)
    val deforested = deforest(bindings, goal)
    println("=> " + deforested.shows)
    println()
  }

}
