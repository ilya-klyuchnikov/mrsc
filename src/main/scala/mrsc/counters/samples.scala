package mrsc.counters

import mrsc._

// TO REMOVE:
trait CounterSyntax extends Syntax[Counter] {
  override val instance = CounterInstanceOrdering
  override def subst(c: Counter, sub: Subst[Counter]) = c
  override def rawRebuildings(c: Counter) = (gens(c) - c) map { (_, emptySubst) }
  override def translate(rb: Rebuilding[Counter]) = rb._1
  override def findSubst(from: Counter, to: Counter) =
    if (instance.lteq(from, to)) Some(emptySubst) else None
  override def size(c: Counter) = c.size
  def gens(c: Counter): List[Counter] = c match {
    case Nil => List(Nil)
    case e :: c1 => for (cg <- genComp(e); gs <- gens(c1)) yield cg :: gs
  }
  def genComp(c: Component): List[Component] = c match {
    case Omega => List(Omega)
    case value => List(Omega, value)
  }
}

trait CounterSemantics extends OperationalSemantics[Counter] {
  val protocol: Protocol
  def applyRules(c: Counter) =
    protocol.rules.filter(_.isDefinedAt(c)).map(_(c))
  override def drive(c: Counter) =
    VariantsDriveStep(applyRules(c) map { (emptyContraction, _) })
  override def isDrivable(c: Counter) =
    protocol.rules.exists(_.isDefinedAt(c))
}

trait MagicGen extends CounterSyntax {
  val l: Int
  override def rawRebuildings(c: Counter) =
    (c.map(e => if (e >= l) Omega else e), emptySubst) :: Nil
}

case class CounterSc(val protocol: Protocol, val l: Int)
  extends CounterSyntax
  with MagicGen
  with LWhistle
  with CounterSemantics
  with SimpleDriving[Counter]
  with Folding[Counter]
  with UnaryWhistle[Counter]
  with CurrentGensOnWhistle[Counter]
  with NoTricks[Counter]

case class CounterSimpleMultiSc(val protocol: Protocol, val l: Int)
  extends CounterSyntax
  with LWhistle
  with CounterSemantics
  with SimpleDriving[Counter]
  with InstanceFolding[Counter]
  with UnaryWhistle[Counter]
  with CurrentGensOnUnaryWhistle[Counter]
  with NoTricks[Counter]

case class CounterTrueMultiSc(val protocol: Protocol, val l: Int)
  extends CounterSyntax
  with LWhistle
  with CounterSemantics
  with PruningDriving[Counter]
  with InstanceFolding[Counter]
  with UnaryWhistle[Counter]
  with ProtocolSafetyAware
  with AlwaysCurrentGensWithUnaryWhistle[Counter]

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
    val sc = CounterMultiSc(protocol, l)
    val consumer = new SimpleGraphConsumer[Counter, Int]
    val builder = new CoGraphBuilder(sc, consumer)
    builder.buildCoGraph(startCounter, NoExtra)
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
    multiScProtocol(protocol, 2, protocol.start)
  }

  verifyProtocol(Synapse)
  verifyProtocol(MSI)
  verifyProtocol(MOSI)
  verifyProtocol(MESI)
}
