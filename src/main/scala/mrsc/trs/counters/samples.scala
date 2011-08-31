package mrsc.trs.counters

import mrsc.core._
import mrsc.trs._

trait LGen extends TRSSyntax[OmegaConf] {
  val l: Int
  override def rebuildings(c: OmegaConf) =
    List(c.map { e => if (e >= l) Omega else e })
}

trait ProtocolSafetyAware extends SafetyAware[OmegaConf, Int] {
  val protocol: Protocol
  override def unsafe(counter: OmegaConf): Boolean =
    protocol.unsafe(counter)
}

case class CounterMachine(val protocol: Protocol, val l: Int)
  extends CountersSyntax
  with LGen
  with LWhistle
  with CountersSemantics
  with RuleDriving[OmegaConf]
  with SimpleInstanceFoldingToAny[OmegaConf, Int]
  with SimpleUnaryWhistle[OmegaConf, Int]
  with SimpleCurrentGensOnWhistle[OmegaConf, Int]

case class CounterMultiMachine(val protocol: Protocol, val l: Int)
  extends CountersSyntax
  with LWhistle
  with CountersSemantics
  with RuleDriving[OmegaConf]
  with SimpleInstanceFoldingToAny[OmegaConf, Int]
  with SimpleUnaryWhistle[OmegaConf, Int]
  with ProtocolSafetyAware
  with SimpleGensWithUnaryWhistle[OmegaConf, Int]

object CounterSamples extends App {

  def graphSize(g: TGraph[_, _]): Int =
    size(g.root)

  def size(n: TNode[_, _]): Int = 1 + n.outs.map(out => size(out.tNode)).sum

  def scProtocol(protocol: Protocol, l: Int): Unit = {
    val machine = CounterMachine(protocol, l)
    val graphs = GraphGenerator(machine, protocol.start)

    for (graph <- graphs if graph.isComplete) {
      val tgraph = Transformations.transpose(graph)
      println("================================")
      println()
      println(tgraph)
      val isSafe = checkSubTree(protocol.unsafe)(tgraph.root)
      println(isSafe)
    }
  }

  def multiScProtocol(protocol: Protocol, l: Int): Unit = {
    val machine = CounterMultiMachine(protocol, l)
    val graphs = GraphGenerator(machine, protocol.start)
    val successGraphs = graphs filter (_.isComplete) map Transformations.transpose
    //val successGraphs = tgraphs.filter { g => checkSubTree(protocol.unsafe)(g.root) }
    if (!successGraphs.isEmpty) {
      val minGraph = successGraphs.minBy(graphSize)
      println(minGraph)
    }
  }

  def checkSubTree(unsafe: OmegaConf => Boolean)(node: TNode[OmegaConf, _]): Boolean =
    !unsafe(node.conf) && node.outs.map(_.tNode).forall(checkSubTree(unsafe))

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
