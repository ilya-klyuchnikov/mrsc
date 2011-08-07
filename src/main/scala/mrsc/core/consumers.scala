package mrsc.core

case class CountResult(countCompleted: Int, countPruned: Int)

class CountGraphConsumer[C, D, E](val maxCount: Int = 10000)
  extends CoGraphConsumer[C, D, E, CountResult] {
  var completed = 0
  var pruned = 0
  lazy val result = CountResult(completed, pruned)

  override def consume(result: Option[CoGraph[C, D, E]]): Unit = {
    result match {
      case None     => pruned = pruned + 1
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

class GraphConsumer[C, D, E] extends CoGraphConsumer[C, D, E, List[Graph[C, D, E]]] {

  var completedGraphs: List[Graph[C, D, E]] = List()
  lazy val result = completedGraphs

  def consume(result: Option[CoGraph[C, D, E]]): Unit =
    for (cg <- result) {
      val graph = Transformations.transpose(cg)
      completedGraphs = graph :: completedGraphs
    }

  override def buildResult() = result
}