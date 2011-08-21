package mrsc.test

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import mrsc.core._
import mrsc.pfp._

object TinyMachine extends Machine[Int, String, Extra[String]] {
  def steps(g: Graph[Int, String, Extra[String]])
    : List[Graph[Int, String, Extra[String]]] =
    g.current.conf match {
    case 0 =>
      List(g.addChildNodes(List((1, "0 -> 1", NoExtra), (2, "0 -> 2", NoExtra))))
    case 1 =>
      List(g.completeCurrentNode())
    case 2 =>
      List(g.rebuild(21, NoExtra))
    case 21 =>
      List(g.rollback(g.current.in.node, -1, NoExtra))
    case -1 =>
      List(g.addChildNodes(List((11, "-1 -> 11", NoExtra))))
    case 11 =>
      List(g.fold(List()))
  }
}

@RunWith(classOf[JUnitRunner])
class BuilderMachineSpec extends mutable.Specification {
  args(sequential = true)

  val graph: TGraph[Int, String, Extra[String]] = {
    val n1 = TNode[Int, String, Extra[String]](conf = 11, extraInfo = NoExtra, outs = List(), back = Some(List()), tPath = List(0))
    val e1 = TEdge[Int, String, Extra[String]](n1, "-1 -> 11")
    val n0 = TNode[Int, String, Extra[String]](conf = -1, extraInfo = NoExtra, outs = List(e1), back = None, tPath = List())
    TGraph(root = n0, leaves = List(n1))
  }

  "GraphBuilder with deterministic machine" should {
    //val consumer = new TGraphConsumer[Int, String, Extra[String]]()
    //val builder = new GraphBuilder(TinyMachine, consumer)
    //builder.buildGraphs(0, NoExtra)
    //val graphs = consumer.result
    
    val producer = GraphProducer(TinyMachine, 0, NoExtra) map Transformations.transpose
    val tgraphs = producer.toList
    //assert(tgraphs == graphs)
    
    "produce just 1 result" in {
      tgraphs.size must_== 1
    }

    "build graph in depth first manner" in {
      tgraphs(0) must_== graph
    }
  }

}