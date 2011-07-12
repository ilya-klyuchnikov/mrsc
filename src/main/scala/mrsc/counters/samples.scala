package mrsc.counters

import mrsc._

trait LGen extends PreSyntax[Counter] {
  val l: Int
  override def rebuildings(c: Counter) = 
    List(c.map(e => if (e >= l) Omega else e))
}

case class CounterSc(val protocol: Protocol, val l: Int)
  extends CounterPreSyntax 
  with LGen
  with LWhistle
  with CounterRewriteSemantics
  with RuleDriving[Counter]
  with SimpleInstanceFoldingToAny[Counter, Int]
  with SimpleUnaryWhistle[Counter, Int]
  with SimpleCurrentGensOnWhistle[Counter, Int]

case class CounterMultiSc(val protocol: Protocol, val l: Int)
  extends CounterPreSyntax
  with LWhistle
  with CounterRewriteSemantics
  with RuleDriving[Counter]
  with SimpleInstanceFoldingToAny[Counter, Int]
  with SimpleUnaryWhistle[Counter, Int]
  with ProtocolSafetyAware
  with SimpleGensWithUnaryWhistle[Counter, Int]

trait ProtocolSafetyAware extends LWhistle {
  val protocol: Protocol
  override def isDangerous(counter: Counter): Boolean =
    super.isDangerous(counter) || !protocol.safe(counter)
}

object CounterSamples extends App {
  
  def graphSize(g: Graph[_, _, _]): Int = 
    size(g.root)
  
  def size(n: Node[_, _, _]): Int = 1 + n.outs.map( out => size(out.node)).sum
  
  def scProtocol(protocol: Protocol, l: Int): Unit = {
    val sc = CounterSc(protocol, l)
    val consumer = new SimpleGraphConsumer[Counter, Int]
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(protocol.start, NoExtra)

    for (graph <- consumer.result) {
      println("================================")
      println()
      println(graph)
      println()
      println(checkSubTree(protocol.safe)(graph.root))
      println()
    }
  }

  def multiScProtocol(protocol: Protocol, l: Int): Unit = {
    val sc = CounterMultiSc(protocol, l)
    val consumer = new SimpleGraphConsumer[Counter, Int]
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(protocol.start, NoExtra)
    val graphs = consumer.result
    
    val successGraphs = graphs.filter{g => checkSubTree(protocol.safe)(g.root)}
    if (!successGraphs.isEmpty) {
      val minGraph = successGraphs.minBy(graphSize)
      println(minGraph)
    }
  }

  def checkSubTree(safe: Counter => Boolean)(node: Node[Counter, _, _]): Boolean =
    safe(node.conf) && node.outs.map(_.node).forall(checkSubTree(safe))
  
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
}
