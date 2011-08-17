package mrsc.trs

import mrsc.core._
import mrsc.pfp._

class SimpleGraphConsumer[C, D] extends GraphConsumer[C, D, Extra[C], List[TGraph[C, D, Extra[C]]]] {
  val description = "counting completed and discarded graphs and showing residual programs"

  var completedGraphs: List[TGraph[C, D, Extra[C]]] = List()
  lazy val result = completedGraphs

  def consume(g: Graph[C, D, Extra[C]]) {
    if (g.isComplete) {
      val tg = Transformations.transpose(g)
      completedGraphs = tg :: completedGraphs
    }
  }

  override def buildResult() = result
}