package mrsc.counters

import mrsc.core._

object Demo extends App {

  def graphSize(g: TGraph[_, _]): Int =
    size(g.root)

  def graphSize(g: SGraph[_, _]): Int =
    g.completeNodes.size

  def size(n: TNode[_, _]): Int = 1 + n.outs.map(out => size(out.node)).sum

  def multiScProtocol(protocol: Protocol, scs: GraphRewriteRules[Conf, Int]*): Unit = {
    
    for (rules <- scs) {
      val graphs = GraphGenerator(rules, protocol.start)
      graphs.size
      println((graphs.consumed, graphs.pruned))
    }

    // the most optimized case
    {
      val rules = MRCountersRules(protocol, 2)
      val graphs = GraphGenerator(rules, protocol.start)

      var minGraph: SGraph[Conf, Int] = null
      var size = Int.MaxValue
      for (graph <- graphs) {
        if (graph.isComplete) {
          if (graphSize(graph) < size) {
            minGraph = graph
            size = graphSize(graph)
            rules.maxSize = size
          }
        }
      }

      println((graphs.consumed, graphs.pruned))
    }
  }

  def demo(protocol: Protocol, scs: GraphRewriteRules[Conf, Int]*): Unit = {
    println(protocol)
    multiScProtocol(protocol, scs: _*)
    println("================================")
  }

  demo(Synapse, MRCountersRules0(Synapse, 2))
  demo(MSI, MRCountersRules0(MSI, 2))
  demo(MOSI, MRCountersRules0(MOSI, 2))
  demo(MESI, MRCountersRules0(MESI, 2))
  demo(MOESI, MRCountersRules0(MOESI, 2))
  demo(Illinois, MRCountersRules0(Illinois, 2))
  demo(Berkley, MRCountersRules0(Berkley, 2))
  demo(Firefly, MRCountersRules0(Firefly, 2))
  
  // many variants here - be patient
  demo(Futurebus)
  
  demo(Xerox, MRCountersRules0(Xerox, 2))
  
  // many variants here - be patient
  demo(Java)
  
  demo(ReaderWriter, MRCountersRules0(ReaderWriter, 2))
  demo(DataRace, MRCountersRules0(DataRace, 2))
}
