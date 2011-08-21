package mrsc.pfp

import mrsc.core._

case class ResidualResult[C](nGraphsCompleted: Int, nGraphsDiscarded: Int, residuals: List[C])

class ResiduatingConsumer[C](residuator: Residuation[C])
  extends GraphConsumer[C, DriveInfo[C], Extra[C], ResidualResult[C]] {
  val description = "counting completed and discarded graphs and showing residual programs"

  var completedGraphsCount = 0
  var unworkableGraphsCount = 0
  var residuals = List[C]()
  lazy val result = ResidualResult(completedGraphsCount, unworkableGraphsCount, residuals)

  def consume(g: Graph[C, DriveInfo[C], Extra[C]]) {
    if (g.isUnworkable)
      unworkableGraphsCount = unworkableGraphsCount + 1
    else {
      assert(g.isComplete)
      completedGraphsCount = completedGraphsCount + 1
      val graph = Transformations.transpose(g)
      val residual = residuator.residuate(graph)
      residuals = residual :: residuals
    }
    if (completedGraphsCount > 1000) {
      throw new ModelingError("too many results")
    }
  }

  override def buildResult() = result
}

class SingleProgramConsumer[C](residuator: Residuation[C])
  extends GraphConsumer[C, DriveInfo[C], Extra[C], C] {
  val description = "I expect one result"

  var residualProgram: C = null.asInstanceOf[C]
  var tgraph: TGraph[C, DriveInfo[C], Extra[C]] = null

  def consume(g: Graph[C, DriveInfo[C], Extra[C]]) {
    if (residualProgram != null)
      throw new Error()
    if (g.isComplete) {
      tgraph = Transformations.transpose(g)
      println(tgraph)
      residualProgram = residuator.residuate(tgraph)
    }
  }

  override def buildResult() = residualProgram
}

case class ResiduatingProducer[C](
    machine: Machine[C, DriveInfo[C], Extra[C]], conf: C, info: Extra[C],
    residuator: Residuation[C])
  extends Iterator[C]
{
  val description = "counting completed and discarded graphs and showing residual programs"

  val graphs = CountingGraphProducer(machine, conf, info, 1000)
  val completeGraphs = graphs filter (_.isComplete)

  def completed = graphs.completed
  def unworkable = graphs.unworkable
  
  def hasNext: Boolean = completeGraphs.hasNext

  def next(): C = {
    val g = completeGraphs.next()
    val t = Transformations.transpose(g)
    residuator.residuate(t)
  }
}

class SingleProgramBuilder[C](residuator: Residuation[C])
  extends ScEngine[C, DriveInfo[C], Extra[C], C] {
  val description = "I expect one result"

  var residualProgram: C = null.asInstanceOf[C]
  var tgraph: TGraph[C, DriveInfo[C], Extra[C]] = null

  def run(machine: Machine[C, DriveInfo[C], Extra[C]], conf: C, info: Extra[C]): C = {
    val graphs = GraphProducer(machine, conf, info) filter (_.isComplete)
    assert(!graphs.isEmpty)
    val g = graphs.next()
    assert(graphs.isEmpty)
    val tgraph = Transformations.transpose(g)
    println(tgraph)
    residuator.residuate(tgraph)
  }
}
