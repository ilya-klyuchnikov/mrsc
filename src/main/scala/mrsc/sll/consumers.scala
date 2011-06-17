package mrsc.sll

import mrsc._

case class CountResult(countCompleted: Int, countPruned: Int)

class CountGraphConsumer[C, D, E] extends CoGraphConsumer[C, D, E, CountResult] {
  val description = "counting completed and pruned graphs"
  var completed = 0
  var pruned = 0
  lazy val result = CountResult(completed, pruned)

  override def consume(result: Option[CoGraph[C, D, E]]): Unit = {
    result match {
      case None => pruned = pruned + 1
      case Some(cg) => completed = completed + 1
    }
    /*
    if (pruned % 1000 == 0 || completed % 1000 == 0) {
      println((completed, pruned))
    }*/
    if (completed > 1000 || pruned > 1000) {
      completed = -1
      pruned = -1
      throw new ModelingError("too many results")
    }
  }
  
  override def buildResult() = result
}

case class ResidualResult(nGraphsCompleted: Int, nGraphsPruned: Int, residuals: List[Expr])

class ResiduatingConsumer extends CoGraphConsumer[Expr, DriveInfo[Expr], Extra, ResidualResult] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedCoGraphsCount = 0
  var prunedCoGraphsCount = 0
  var residuals = List[Expr]()
  lazy val result = ResidualResult(completedCoGraphsCount, prunedCoGraphsCount, residuals)

  def consume(result: Option[CoGraph[Expr, DriveInfo[Expr], Extra]]): Unit = {
    result match {
      case None =>
        prunedCoGraphsCount = prunedCoGraphsCount + 1
      case Some(cg) =>
        completedCoGraphsCount = completedCoGraphsCount + 1
        val graph = Transformations.transpose(cg)
        val residual = new NaiveResiduator().residuate(graph)
        residuals = residual :: residuals
    }
    if (completedCoGraphsCount > 1000) {
      throw new ModelingError("too many results")
    }
  }

   override def buildResult() = result 
}

class SingleProgramConsumer extends CoGraphConsumer[Expr, DriveInfo[Expr], Extra, Expr] {
  val description = "I expect one result"

  var residualProgram: Expr = null
  var graph: Graph[Expr, DriveInfo[Expr], Extra] = null
  
  lazy val residualTask: SLLTask = SLLExpressions.expr2Task(residualProgram)

  def consume(result: Option[CoGraph[Expr, DriveInfo[Expr], Extra]]): Unit = {
    result match {
      case Some(cg) if residualProgram == null =>
        graph = Transformations.transpose(cg)
        residualProgram = new NaiveResiduator().residuate(graph)
      case _ =>
        throw new Error()
    }
  }

  override def buildResult() = residualProgram 
}