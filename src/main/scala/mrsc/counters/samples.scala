package mrsc.counters

import mrsc._

case class CounterSc(val rules: List[TransitionRule], val l: Int)
  extends GenericMultiMachine[Counter, DriveInfo[Counter], Extra]
  with CounterSyntax
  with MagicGen
  with MagicWhistle
  with CounterSemantics
  with SimpleDriving[Counter]
  with Folding[Counter]
  with UnaryWhistle[Counter]
  with CurrentGensOnWhistle[Counter]
  with NoTricks[Counter]

case class CounterMultiSc(val rules: List[TransitionRule], val l: Int)
  extends GenericMultiMachine[Counter, DriveInfo[Counter], Extra]
  with CounterSyntax
  with MagicWhistle
  with CounterSemantics
  with SimpleDriving[Counter]
  with InstanceFolding[Counter]
  with UnaryWhistle[Counter]
  with CurrentGensOnUnaryWhistle[Counter]
  with NoTricks[Counter]

object CounterSamples extends App {
  def scProtocol(rules: List[TransitionRule], safe: Counter => Boolean, l: Int, startCounter: Counter): Unit = {
    val sc = CounterSc(rules, l)
    val consumer = new GraphConsumer[Counter]
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(startCounter, NoExtra)

    for (graph <- consumer.result) {
      println("================================")
      println()
      println(graph)
      println()
      println(checkSubTree(safe)(graph.root))
      println()
    }
  }

  def multiScProtocol(rules: List[TransitionRule], safe: Counter => Boolean, l: Int, startCounter: Counter): Unit = {
    val sc = CounterMultiSc(rules, l)
    val consumer = new GraphConsumer[Counter]
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(startCounter, NoExtra)

    for (graph <- consumer.result) {
      println("================================")
      println()
      println(graph)
      println()
      println(checkSubTree(safe)(graph.root))
      println()
    }
  }

  def checkSubTree(safe: Counter => Boolean)(node: Node[Counter, _, _]): Boolean =
    safe(node.conf) && node.outs.map(_.node).forall(checkSubTree(safe))

  scProtocol(protocols.MESI, protocols.safeMESI, 0, List(ϖ, 0, 0, 0))
  scProtocol(protocols.MESI, protocols.safeMESI, 1, List(ϖ, 0, 0, 0))
  scProtocol(protocols.MESI, protocols.safeMESI, 2, List(ϖ, 0, 0, 0))

  //multiScProtocol(protocols.MESI, protocols.safeMESI, 2, List(ϖ, 0, 0, 0))
}
