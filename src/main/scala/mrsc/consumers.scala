package mrsc

case class CountResult(countCompleted: Int, countPruned: Int)

class CountGraphConsumer[C, D, E](val maxCount: Int = 10000)
  extends CoGraphConsumer[C, D, E, CountResult] {
  val description = "counting completed and pruned graphs"
  var completed = 0
  var pruned = 0
  lazy val result = CountResult(completed, pruned)

  override def consume(result: Option[CoGraph[C, D, E]]): Unit = {
    result match {
      case None => pruned = pruned + 1
      case Some(cg) => completed = completed + 1
    }
    if (completed > maxCount || pruned > maxCount) {
      completed = -1
      pruned = -1
      throw new ModelingError("too many results")
    }
  }

  override def buildResult() = result
}

case class ResidualResult[C](nGraphsCompleted: Int, nGraphsPruned: Int, residuals: List[C])

class ResiduatingConsumer[C](residuator: Residuation[C])
  extends CoGraphConsumer[C, DriveInfo[C], Extra, ResidualResult[C]] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedCoGraphsCount = 0
  var prunedCoGraphsCount = 0
  var residuals = List[C]()
  lazy val result = ResidualResult(completedCoGraphsCount, prunedCoGraphsCount, residuals)

  def consume(result: Option[CoGraph[C, DriveInfo[C], Extra]]): Unit = {
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

class GraphConsumer[C, D, E] extends CoGraphConsumer[C, D, E, List[Graph[C, D, E]]] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedGraphs: List[Graph[C, D, E]] = List()
  lazy val result = completedGraphs

  def consume(result: Option[CoGraph[C, D, E]]): Unit = 
    for (cg <- result) {
      val graph = Transformations.transpose(cg)
      completedGraphs = graph :: completedGraphs
    }

  override def buildResult() = result
}

class SimpleGraphConsumer[C, D] extends CoGraphConsumer[C, D, Extra, List[Graph[C, D, Extra]]] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedGraphs: List[Graph[C, D, Extra]] = List()
  lazy val result = completedGraphs

  def consume(result: Option[CoGraph[C, D, Extra]]): Unit = 
    for (cg <- result) {
      val graph = Transformations.transpose(cg)
      completedGraphs = graph :: completedGraphs
    }

  override def buildResult() = result
}

class SingleProgramConsumer[C](residuator: Residuation[C])
  extends CoGraphConsumer[C, DriveInfo[C], Extra, C] {
  val description = "I expect one result"

  var residualProgram: C = null.asInstanceOf[C]
  var graph: Graph[C, DriveInfo[C], Extra] = null

  def consume(result: Option[CoGraph[C, DriveInfo[C], Extra]]): Unit = {
    result match {
      case Some(cg) if residualProgram == null =>
        graph = Transformations.transpose(cg)
        residualProgram = residuator.residuate(graph)
      case _ =>
        throw new Error()
    }
  }

  override def buildResult() = residualProgram
}