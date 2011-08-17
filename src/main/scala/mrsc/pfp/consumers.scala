package mrsc.pfp

import mrsc.core._

case class ResidualResult[C](nGraphsCompleted: Int, nGraphsPruned: Int, residuals: List[C])

class ResiduatingConsumer[C](residuator: Residuation[C])
  extends CoGraphConsumer[C, DriveInfo[C], Extra[C], ResidualResult[C]] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedCoGraphsCount = 0
  var unworkableCoGraphsCount = 0
  var residuals = List[C]()
  lazy val result = ResidualResult(completedCoGraphsCount, unworkableCoGraphsCount, residuals)

  def consume(cg: PartialCoGraph[C, DriveInfo[C], Extra[C]]) {
    if (cg.isUnworkable)
      unworkableCoGraphsCount = unworkableCoGraphsCount + 1
    else {
      completedCoGraphsCount = completedCoGraphsCount + 1
      val graph = Transformations.transpose(cg)
      val residual = residuator.residuate(graph)
      residuals = residual :: residuals
    }
    if (completedCoGraphsCount > 1000) {
      throw new ModelingError("too many results")
    }
  }

  override def buildResult() = result
}

class SingleProgramConsumer[C](residuator: Residuation[C])
  extends CoGraphConsumer[C, DriveInfo[C], Extra[C], C] {
  val description = "I expect one result"

  var residualProgram: C = null.asInstanceOf[C]
  var graph: Graph[C, DriveInfo[C], Extra[C]] = null

  def consume(cg: PartialCoGraph[C, DriveInfo[C], Extra[C]]) {
    if (residualProgram != null)
      throw new Error()
    if (cg.isComplete) {
      graph = Transformations.transpose(cg)
      println(graph)
      residualProgram = residuator.residuate(graph)
    }
  }

  override def buildResult() = residualProgram
}