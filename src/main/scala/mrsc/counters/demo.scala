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
      println("* " + (graphs.consumed, graphs.pruned, graphs.stepsDone))
    }

    // the most optimized case
    {
      val rules = MRCountersRules5(protocol, 2)
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

      println("* " + (graphs.consumed, graphs.pruned, graphs.stepsDone))
    }
  }

  def demo(protocol: Protocol, scs: GraphRewriteRules[Conf, Int]*): Unit = {
    println("# " + protocol)
    multiScProtocol(protocol, scs: _*)
    println()
  }

  demo(Synapse, MRCountersRules1(Synapse, 2), MRCountersRules2(Synapse, 2), MRCountersRules3(Synapse, 2), MRCountersRules4(Synapse, 2))
  demo(MSI, MRCountersRules1(MSI, 2), MRCountersRules2(MSI, 2), MRCountersRules3(MSI, 2), MRCountersRules4(MSI, 2))
  demo(MOSI, MRCountersRules1(MOSI, 2), MRCountersRules2(MOSI, 2), MRCountersRules3(MOSI, 2), MRCountersRules4(MOSI, 2))
  demo(MESI, MRCountersRules1(MESI, 2), MRCountersRules2(MESI, 2), MRCountersRules3(MESI, 2), MRCountersRules4(MESI, 2))
  demo(MOESI, MRCountersRules1(MOESI, 2), MRCountersRules2(MOESI, 2), MRCountersRules3(MOESI, 2), MRCountersRules4(MOESI, 2))
  demo(Illinois, MRCountersRules1(Illinois, 2), MRCountersRules2(Illinois, 2), MRCountersRules3(Illinois, 2), MRCountersRules4(Illinois, 2))
  demo(Berkley, MRCountersRules1(Berkley, 2), MRCountersRules2(Berkley, 2), MRCountersRules3(Berkley, 2), MRCountersRules4(Berkley, 2))
  demo(Firefly, MRCountersRules1(Firefly, 2), MRCountersRules2(Firefly, 2), MRCountersRules3(Firefly, 2), MRCountersRules4(Firefly, 2))
  
  // There are really many variants here
  // so we log only the most optimized supercompiler
  demo(Futurebus)
  
  demo(Xerox, MRCountersRules1(Xerox, 2), MRCountersRules2(Xerox, 2), MRCountersRules3(Xerox, 2), MRCountersRules4(Xerox, 2))
  
  // There are really many variants here
  // so we log only the most optimized supercompiler
  demo(Java)
  
  demo(ReaderWriter, MRCountersRules1(ReaderWriter, 2), MRCountersRules2(ReaderWriter, 2), MRCountersRules3(ReaderWriter, 2), MRCountersRules4(ReaderWriter, 2))
  demo(DataRace, MRCountersRules1(DataRace, 2), MRCountersRules2(DataRace, 2), MRCountersRules3(DataRace, 2), MRCountersRules4(DataRace, 2))
}
