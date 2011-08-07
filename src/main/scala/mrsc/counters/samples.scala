package mrsc.counters

import mrsc.core._
import mrsc.pfp._

trait LGen extends PreSyntax[OmegaConf] {
  val l: Int
  override def rebuildings(c: OmegaConf) =
    List(c.map { e => if (e >= l) Omega else e })
}

case class CounterSc(val protocol: Protocol, val l: Int)
  extends CountersPreSyntax
  with LGen
  with LWhistle
  with CountersSemantics
  with RuleDriving[OmegaConf]
  with SimpleInstanceFoldingToAny[OmegaConf, Int]
  with SimpleUnaryWhistle[OmegaConf, Int]
  with SimpleCurrentGensOnWhistle[OmegaConf, Int]

case class CounterMultiSc(val protocol: Protocol, val l: Int)
  extends CountersPreSyntax
  with LWhistle
  with CountersSemantics
  with RuleDriving[OmegaConf]
  with SimpleInstanceFoldingToAny[OmegaConf, Int]
  with SimpleUnaryWhistle[OmegaConf, Int]
  with ProtocolSafetyAware
  with SimpleGensWithUnaryWhistle[OmegaConf, Int]

trait ProtocolSafetyAware extends LWhistle {
  val protocol: Protocol
  override def unsafe(counter: OmegaConf): Boolean =
    super.unsafe(counter) || protocol.unsafe(counter)
}

object CounterSamples extends App {

  def graphSize(g: Graph[_, _, _]): Int =
    size(g.root)

  def size(n: Node[_, _, _]): Int = 1 + n.outs.map(out => size(out.node)).sum

  def scProtocol(protocol: Protocol, l: Int): Unit = {
    val sc = CounterSc(protocol, l)
    val consumer = new SimpleGraphConsumer[OmegaConf, Int]
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(protocol.start, NoExtra)

    for (graph <- consumer.result) {
      println("================================")
      println()
      println(graph)
      println()
      println(checkSubTree(protocol.unsafe)(graph.root))
      println()
    }
  }

  def multiScProtocol(protocol: Protocol, l: Int): Unit = {
    val sc = CounterMultiSc(protocol, l)
    val consumer = new SimpleGraphConsumer[OmegaConf, Int]
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(protocol.start, NoExtra)
    val graphs = consumer.result

    val successGraphs = graphs.filter { g => checkSubTree(protocol.unsafe)(g.root) }
    if (!successGraphs.isEmpty) {
      val minGraph = successGraphs.minBy(graphSize)
      println(minGraph)
    }
  }

  def checkSubTree(unsafe: OmegaConf => Boolean)(node: Node[OmegaConf, _, _]): Boolean =
    !unsafe(node.conf) && node.outs.map(_.node).forall(checkSubTree(unsafe))

  def verifyProtocol(protocol: Protocol, findMinimalProof: Boolean = true): Unit = {
    println()
    println(protocol)
    scProtocol(protocol, 2)
    if (findMinimalProof) {
      multiScProtocol(protocol, 2)
    } else {
      println("skipping quest for minimal proof")
    }
  }

  verifyProtocol(Synapse)
  verifyProtocol(MSI)
  verifyProtocol(MOSI)
  verifyProtocol(MESI)
  verifyProtocol(MOESI)
  verifyProtocol(Illinois)
  verifyProtocol(Berkley)
  verifyProtocol(Firefly)
  verifyProtocol(Futurebus, findMinimalProof = false) // too many variants here
  verifyProtocol(Xerox)
  verifyProtocol(Java, findMinimalProof = false) // too many variants here
  verifyProtocol(ReaderWriter)
  verifyProtocol(DataRace)
}
