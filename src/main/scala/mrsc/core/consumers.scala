package mrsc.core

case class CountingGraphProducer[C, D, E](
  machine: Machine[C, D, E], conf: C, info: E,
  maxCount: Int = 2000)
  extends Iterator[Graph[C, D, E]] {

  var completed = 0
  var unworkable = 0

  private val graphs = GraphGenerator(machine, conf, info)

  def hasNext: Boolean = graphs.hasNext

  def next(): Graph[C, D, E] = {
    val g = graphs.next()

    if (g.isUnworkable)
      unworkable += 1
    else
      completed += 1

    if (completed > maxCount) {
      completed = -1
      unworkable = -1
      throw new ModelingError("too many results")
    }
    g
  }
}
