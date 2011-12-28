package mrsc.counters

import mrsc.core._

object CounterSamples extends App {

  def graphSize(g: TGraph[_, _]): Int =
    size(g.root)

  def graphSize(g: SGraph[_, _]): Int =
    g.completeNodes.size

  def size(n: TNode[_, _]): Int = 1 + n.outs.map(out => size(out.node)).sum

  def scProtocol(protocol: Protocol, l: Int): Unit = {

    val rules = SRCountersRules(protocol, l)
    val graphs = GraphGenerator(rules, protocol.start)

    for (graph <- graphs if graph.isComplete) {
      val tgraph = Transformations.transpose(graph)
      println("================================")
      println(graphSize(tgraph))
      println(tgraph)
      val isSafe = checkSubTree(protocol.unsafe)(tgraph.root)
      println(isSafe)
    }
  }

  def multiScProtocol(protocol: Protocol, l: Int): Unit = {
    val rules = MRCountersRules(protocol, l)
    val graphs = GraphGenerator(rules, protocol.start)

    var minGraph: SGraph[Conf, Int] = null
    var size = 1000000
    for (graph <- graphs) {
      if (graph.isComplete) {
        if (graphSize(graph) < size) {
          minGraph = graph
          size = graphSize(graph)
        }
      }
    }
    
    if (minGraph != null) {
      val tgraph = Transformations.transpose(minGraph)
      println(graphSize(tgraph))
      println(tgraph)
    }
  }

  def checkSubTree(unsafe: Conf => Boolean)(node: TNode[Conf, _]): Boolean =
    !unsafe(node.conf) && node.outs.map(_.node).forall(checkSubTree(unsafe))

  def verifyProtocol(protocol: Protocol): Unit = {
    println()
    println(protocol)
    scProtocol(protocol, 2)
    multiScProtocol(protocol, 2)
  }

  verifyProtocol(Synapse)
  verifyProtocol(MSI)
  verifyProtocol(MOSI)
  verifyProtocol(MESI)
  verifyProtocol(MOESI)
  verifyProtocol(Illinois)
  verifyProtocol(Berkley)
  verifyProtocol(Firefly)
  // many variants here - be patient
  verifyProtocol(Futurebus) 
  verifyProtocol(Xerox)
  // many variants here - be patient
  verifyProtocol(Java) 
  verifyProtocol(ReaderWriter)
  verifyProtocol(DataRace)
}
