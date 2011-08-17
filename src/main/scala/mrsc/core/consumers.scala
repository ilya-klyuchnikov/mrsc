package mrsc.core

case class CountResult(countCompleted: Int, countUnworkable: Int)

class CountGraphConsumer[C, D, E](val maxCount: Int = 10000)
  extends CoGraphConsumer[C, D, E, CountResult] {
  var completed = 0
  var unworkable = 0
  lazy val result = CountResult(completed, unworkable)

  override def consume(cg: PartialCoGraph[C, D, E]) {
    if (cg.isUnworkable)
      unworkable = unworkable + 1
    else
      completed = completed + 1

    if (completed > maxCount) {
      completed = -1
      unworkable = -1
      throw new ModelingError("too many results")
    }
  }

  override def buildResult() = result
}

class GraphConsumer[C, D, E] extends CoGraphConsumer[C, D, E, List[Graph[C, D, E]]] {

  var completedGraphs: List[Graph[C, D, E]] = List()
  lazy val result = completedGraphs

  def consume(cg: PartialCoGraph[C, D, E]) {
    if (cg.isComplete) {
      val graph = Transformations.transpose(cg.toCoGraph())
      completedGraphs = graph :: completedGraphs
    }
  }

  override def buildResult() = result
}