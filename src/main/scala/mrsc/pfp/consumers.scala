package mrsc.pfp

import mrsc.core._

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

case class SingleProgramProducer[C](residuator: Residuation[C]) {
  val description = "I expect one result"

  var residualProgram: C = null.asInstanceOf[C]
  var tgraph: TGraph[C, DriveInfo[C], Extra[C]] = null

  def apply(machine: Machine[C, DriveInfo[C], Extra[C]], conf: C, info: Extra[C]): C = {
    val graphs = GraphProducer(machine, conf, info) filter (_.isComplete)
    assert(!graphs.isEmpty)
    val g = graphs.next()
    assert(graphs.isEmpty)
    val tgraph = Transformations.transpose(g)
    println(tgraph)
    residuator.residuate(tgraph)
  }
}
