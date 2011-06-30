package mrsc.counters

import mrsc._

case class CounterSc(val rules: List[TransitionRule], val l: Int)
  extends GenericMultiMachine[Counter, DriveInfo[Counter], Extra]
  with CounterSyntax
  with CounterMagic
  with CounterSemantics
  with SimpleDriving[Counter]
  with Folding[Counter]
  with UnaryWhistle[Counter]
  with CurrentGensOnWhistle[Counter]
  with NoTricks[Counter]

object CounterSamples extends App {
  def scProtocol(rules: List[TransitionRule], safe: Counter => Boolean, l: Int, startCounter: Counter): Unit = {
    val sc = CounterSc(rules, l)
    val consumer = new GraphConsumer[Counter]
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(startCounter, NoExtra)

    for (graph <- consumer.result) {
      println("<<<>>>")
      println(graph)
      println(checkSubTree(safe)(graph.root))
    }
  }

  def checkSubTree(safe: Counter => Boolean)(node: Node[Counter, _, _]): Boolean =
    safe(node.conf) && node.outs.map(_.node).forall(checkSubTree(safe))

  scProtocol(protocols.MESI, protocols.safeMESI, 0, List(Omega, 0, 0, 0))
  scProtocol(protocols.MESI, protocols.safeMESI, 1, List(Omega, 0, 0, 0))
  scProtocol(protocols.MESI, protocols.safeMESI, 2, List(Omega, 0, 0, 0))
}
