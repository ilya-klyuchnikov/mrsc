package mrsc.core

case class CountResult(countCompleted: Int, countUnworkable: Int)

class CountGraphConsumer[C, D, E](val maxCount: Int = 10000)
  extends GraphConsumer[C, D, E, CountResult] {
  var completed = 0
  var unworkable = 0
  lazy val result = CountResult(completed, unworkable)

  override def consume(g: Graph[C, D, E]) {
    if (g.isUnworkable)
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

class TGraphConsumer[C, D, E] extends GraphConsumer[C, D, E, List[TGraph[C, D, E]]] {

  var completedGraphs: List[TGraph[C, D, E]] = List()
  lazy val result = completedGraphs

  def consume(g: Graph[C, D, E]) {
    if (g.isComplete) {
      val tg = Transformations.transpose(g)
      completedGraphs = tg :: completedGraphs
    }
  }

  override def buildResult() = result
}

case class TGraphProducer[C, D, E](machine: Machine[C, D, E]) {
  val producer = GraphProducer(machine)
  def apply(conf: C, info: E) = {
    producer(conf, info) map Transformations.transpose
  }
}