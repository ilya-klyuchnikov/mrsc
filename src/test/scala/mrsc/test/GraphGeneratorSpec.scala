package mrsc.test

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import mrsc.core._
import mrsc.pfp._

object TinyMachine extends Machine[Int, String, Extra[String]] {
  def steps(g: Graph[Int, String, Extra[String]]): List[S] = {
    g.current.conf match {
      case 0 =>
        List(AddChildNodes(List((1, "0 -> 1", NoExtra), (2, "0 -> 2", NoExtra))))
      case 1 =>
        List(CompleteCurrentNode())
      case 2 =>
        List(Rebuild(21, NoExtra))
      case 21 =>
        List(Rollback(g.current.in.node, -1, NoExtra))
      case -1 =>
        List(AddChildNodes(List((11, "-1 -> 11", NoExtra))))
      case 11 =>
        List(Fold(g.current.in.node))
    }
  }
}

@RunWith(classOf[JUnitRunner])
class GraphGeneratorSpec extends mutable.Specification {
  args(sequential = true)

  val graph: TGraph[Int, String, Extra[String]] = {
    val n1 = TNode[Int, String, Extra[String]](conf = 11, extraInfo = NoExtra, outs = List(), back = Some(List()), tPath = List(0))
    val e1 = TEdge[Int, String, Extra[String]](n1, "-1 -> 11")
    val n0 = TNode[Int, String, Extra[String]](conf = -1, extraInfo = NoExtra, outs = List(e1), back = None, tPath = List())
    TGraph(root = n0, leaves = List(n1))
  }

  "GraphGenerator with deterministic machine" should {

    val tgraphs = GraphGenerator(TinyMachine, 0, NoExtra) map Transformations.transpose toList

    "produce just 1 result" in {
      tgraphs.size must_== 1
    }

    "build graph in depth first manner" in {
      tgraphs(0) must_== graph
    }
  }

}