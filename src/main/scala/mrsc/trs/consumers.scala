package mrsc.trs

import mrsc.core._
import mrsc.pfp._

class SimpleGraphConsumer[C, D] extends CoGraphConsumer[C, D, Extra[C], List[Graph[C, D, Extra[C]]]] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedGraphs: List[Graph[C, D, Extra[C]]] = List()
  lazy val result = completedGraphs

  def consume(cg: PartialCoGraph[C, D, Extra[C]]) {
    if (cg.isComplete) {
      val graph = Transformations.transpose(cg.toCoGraph())
      completedGraphs = graph :: completedGraphs
    }
  }

  override def buildResult() = result
}