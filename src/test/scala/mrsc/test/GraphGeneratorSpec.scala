package mrsc.test

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import mrsc.core._
import mrsc.pfp._

object TinyMachine
  extends Machine[Int, String] {

  def steps(g: SGraph[Int, String]): List[S] = {
    g.current.conf match {
      case 0 =>
        List(AddChildNodesStep(List((1, "0 -> 1"), (2, "0 -> 2"))))
      case 1 =>
        List(CompleteCurrentNodeStep())
      case 2 =>
        List(RebuildStep(21))
      case 21 =>
        List(RollbackStep(g.current.in.node, -1))
      case -1 =>
        List(AddChildNodesStep(List((11, "-1 -> 11"))))
      case 11 =>
        List(FoldStep(g.current.in.node))
    }
  }
}

@RunWith(classOf[JUnitRunner])
class GraphGeneratorSpec extends mutable.Specification {
  args(sequential = true)

  val graph: TGraph[Int, String] = {
    val n1 = TNode[Int, String](conf = 11, outs = List(), base = Some(List()), tPath = List(0))
    val e1 = TEdge[Int, String](n1, "-1 -> 11")
    val n0 = TNode[Int, String](conf = -1, outs = List(e1), base = None, tPath = List())
    TGraph(root = n0, leaves = List(n1))
  }

  "GraphGenerator with deterministic machine" should {

    val tgraphs = GraphGenerator(TinyMachine, 0) map Transformations.transpose toList

    "produce just 1 result" in {
      tgraphs.size must_== 1
    }

    "build graph in depth first manner" in {
      tgraphs(0) must_== graph
    }
  }

}