package mrsc.pfp

import mrsc.core._

case class ResidualResult[C](nGraphsCompleted: Int, nGraphsPruned: Int, residuals: List[C])

class ResiduatingConsumer[C](residuator: Residuation[C])
  extends CoGraphConsumer[C, DriveInfo[C], Extra[C], ResidualResult[C]] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedCoGraphsCount = 0
  var prunedCoGraphsCount = 0
  var residuals = List[C]()
  lazy val result = ResidualResult(completedCoGraphsCount, prunedCoGraphsCount, residuals)

  def consume(result: Option[CoGraph[C, DriveInfo[C], Extra[C]]]): Unit = {
    result match {
      case None =>
        prunedCoGraphsCount = prunedCoGraphsCount + 1
      case Some(cg) =>
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

  def consume(result: Option[CoGraph[C, DriveInfo[C], Extra[C]]]): Unit = {
    result match {
      case Some(cg) if residualProgram == null =>
        graph = Transformations.transpose(cg)
        println(graph)
        residualProgram = residuator.residuate(graph)
      case _ =>
        throw new Error()
    }
  }

  override def buildResult() = residualProgram
}