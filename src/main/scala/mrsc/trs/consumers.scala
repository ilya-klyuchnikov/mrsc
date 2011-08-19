package mrsc.trs

import mrsc.core._
import mrsc.pfp._

class SimpleGraphConsumer[C, D] extends GraphConsumer[C, D, Unit, List[TGraph[C, D, Unit]]] {
  val description = "counting completed and discarded graphs and showing residual programs"

  var completedGraphs: List[TGraph[C, D, Unit]] = List()
  lazy val result = completedGraphs

  def consume(g: Graph[C, D, Unit]) {
    if (g.isComplete) {
      val tg = Transformations.transpose(g)
      completedGraphs = tg :: completedGraphs
    }
  }

  override def buildResult() = result
}