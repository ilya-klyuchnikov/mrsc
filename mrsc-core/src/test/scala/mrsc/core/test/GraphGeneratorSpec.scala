package mrsc.core.test

import scala.language.postfixOps

import org.scalatest.FunSpec

import mrsc.core._

object TinyRules extends GraphRewriteRules[Int, String] {

  def steps(g: SGraph[Int, String]): List[S] = {
    g.current.conf match {
      case 0 =>
        List(AddChildNodesStep(List((1, "0 -> 1"), (2, "0 -> 2"))))
      case 1 =>
        List(CompleteCurrentNodeStep())
      case 2 =>
        List(RebuildStep(21))
      case 21 =>
        List(RollbackStep(g.current.in.node.sPath, -1))
      case -1 =>
        List(AddChildNodesStep(List((11, "-1 -> 11"))))
      case 11 =>
        List(FoldStep(g.current.in.node.sPath))
    }
  }
}

class GraphGeneratorSpec extends FunSpec {

  val graph: TGraph[Int, String] = {
    val n1 = TNode[Int, String](conf = 11, outs = List(), base = Some(List()), tPath = List(0))
    val e1 = TEdge[Int, String](n1, "-1 -> 11")
    val n0 = TNode[Int, String](conf = -1, outs = List(e1), base = None, tPath = List())
    TGraph(root = n0, leaves = List(n1))
  }

  describe("GraphGenerator with deterministic rules") {

    val tgraphs = GraphGenerator(TinyRules, 0) map Transformations.transpose toList

    it("should produce just 1 result") {
      assert(tgraphs.size === 1)
    }

    it("should build graph in depth first manner") {
      assert(tgraphs(0) === graph)
    }
  }

}
