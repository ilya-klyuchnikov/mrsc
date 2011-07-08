package mrsc.counters

import mrsc._

case class CounterSc(val protocol: Protocol, val l: Int)
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

case class CounterSimpleMultiSc(val protocol: Protocol, val l: Int)
  extends GenericMultiMachine[Counter, DriveInfo[Counter], Extra]
  with CounterSyntax
  with MagicWhistle
  with CounterSemantics
  with SimpleDriving[Counter]
  with InstanceFolding[Counter]
  with UnaryWhistle[Counter]
  with CurrentGensOnUnaryWhistle[Counter]
  with NoTricks[Counter]

case class CounterTrueMultiSc(val protocol: Protocol, val l: Int)
  extends GenericMultiMachine[Counter, DriveInfo[Counter], Extra]
  with CounterSyntax
  with MagicWhistle
  with CounterSemantics
  with PruningDriving[Counter]
  with InstanceFolding[Counter]
  with UnaryWhistle[Counter]
  with ProtocolSafetyAware
  with AlwaysCurrentGensWithUnaryWhistle[Counter]
  with NoTricks[Counter]

trait ProtocolSafetyAware extends MagicWhistle {
  val protocol: Protocol
  override def isDangerous(counter: Counter): Boolean = 
    super.isDangerous(counter) || !protocol.safe(counter)
}


object CounterSamples extends App {
  def scProtocol(protocol: Protocol, l: Int, startCounter: Counter): Unit = {
    val sc = CounterSc(protocol, l)
    val consumer = new GraphConsumer[Counter]
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(startCounter, NoExtra)

    for (graph <- consumer.result) {
      println("================================")
      println()
      println(graph)
      println()
      println(checkSubTree(protocol.safe)(graph.root))
      println()
    }
  }

  def simpleMultiScProtocol(protocol: Protocol, l: Int, startCounter: Counter): Unit = {
    val sc = CounterSimpleMultiSc(protocol, l)
    val consumer = new GraphConsumer[Counter]
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(startCounter, NoExtra)

    for (graph <- consumer.result) {
      println("================================")
      println()
      println(graph)
      println()
      println(checkSubTree(protocol.safe)(graph.root))
      println()
    }
  }
  
  def multiScProtocol(protocol: Protocol, l: Int, startCounter: Counter): Unit = {
    val sc = CounterTrueMultiSc(protocol, l)
    val consumer = new GraphConsumer[Counter]
    //val consumer = new CountGraphConsumer[Counter, DriveInfo[Counter], Extra](100000000)
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(startCounter, NoExtra)
    println(consumer.result)

    
    for (graph <- consumer.result) {
      println("================================")
      println()
      println(graph)
      println()
      println(checkSubTree(protocol.safe)(graph.root))
      println()
    }
  }

  def checkSubTree(safe: Counter => Boolean)(node: Node[Counter, _, _]): Boolean =
    safe(node.conf) && node.outs.map(_.node).forall(checkSubTree(safe))

  //scProtocol(MESI, 0, List(ϖ, 0, 0, 0))
  //scProtocol(MESI, 1, List(ϖ, 0, 0, 0))
  scProtocol(MESI, 2, List(ϖ, 0, 0, 0))

  //simpleMultiScProtocol(MESI, 2, List(ϖ, 0, 0, 0))
  
  println("***")
  multiScProtocol(MESI, 1, List(ϖ, 0, 0, 0))
  multiScProtocol(MESI, 2, List(ϖ, 0, 0, 0))
}
