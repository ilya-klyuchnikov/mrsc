package mrsc.pfp

import mrsc.core._

case class ResidualResult[C](nGraphsCompleted: Int, nGraphsDiscarded: Int, residuals: List[C])

class ResiduatingConsumer[C](residuator: Residuation[C])
  extends GraphConsumer[C, DriveInfo[C], Extra[C], ResidualResult[C]] {
  val description = "counting completed and discarded graphs and showing residual programs"

  var completedGraphsCount = 0
  var unworkableGraphsCount = 0
  var residuals = List[C]()
  lazy val result = ResidualResult(completedGraphsCount, unworkableGraphsCount, residuals)

  def consume(g: Graph[C, DriveInfo[C], Extra[C]]) {
    if (g.isUnworkable)
      unworkableGraphsCount = unworkableGraphsCount + 1
    else {
      completedGraphsCount = completedGraphsCount + 1
      val graph = Transformations.transpose(g)
      val residual = residuator.residuate(graph)
      residuals = residual :: residuals
    }
    if (completedGraphsCount > 1000) {
      throw new ModelingError("too many results")
    }
  }

  override def buildResult() = result
}

class SingleProgramConsumer[C](residuator: Residuation[C])
  extends GraphConsumer[C, DriveInfo[C], Extra[C], C] {
  val description = "I expect one result"

  var residualProgram: C = null.asInstanceOf[C]
  var tgraph: TGraph[C, DriveInfo[C], Extra[C]] = null

  def consume(g: Graph[C, DriveInfo[C], Extra[C]]) {
    if (residualProgram != null)
      throw new Error()
    if (g.isComplete) {
      tgraph = Transformations.transpose(g)
      println(tgraph)
      residualProgram = residuator.residuate(tgraph)
    }
  }

  override def buildResult() = residualProgram
}