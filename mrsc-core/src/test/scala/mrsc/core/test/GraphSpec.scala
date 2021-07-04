package mrsc.core.test

import org.scalatest.FunSpec

import mrsc.core._

class GraphSpec extends FunSpec {

  def mkGraph(
      root: SNode[Int, Int] = null,
      nodes: List[SNode[Int, Int]] = List(),
      leaves: List[SNode[Int, Int]] = List(),
  ): SGraph[Int, Int] = SGraph(List(), leaves, nodes)

  var graph1, graph1a, graph2, graph2a: SGraph[Int, Int] = _
  var tgraph1, tgraph2: TGraph[Int, Int] = _

  describe("SNodes and SEdges are created from the top down") {

    val cn0 = SNode[Int, Int](conf = 0, in = null, base = None, sPath = List())
    val ce1 = SEdge[Int, Int](cn0, 1)
    val cn1 = SNode[Int, Int](conf = 1, in = ce1, base = None, sPath = List(0))
    val ce2 = SEdge[Int, Int](cn0, 2)
    val cn2 = SNode[Int, Int](conf = 2, in = ce2, base = None, sPath = List(1))
    val ce3 = SEdge[Int, Int](cn1, 3)
    val cn3 = SNode[Int, Int](conf = 3, in = ce3, base = Some(List(1, 0)), sPath = List(0, 0))
    val ce4 = SEdge[Int, Int](cn1, 4)
    val cn4 = SNode[Int, Int](conf = 4, in = ce4, base = Some(List()), sPath = List(1, 0))

    it("the top subtrees can be reused for constructing different SGraphs") {
      graph1 = mkGraph(root = cn0, nodes = List(cn0, cn1, cn2), leaves = List(cn1, cn2))
      graph2 = mkGraph(root = cn0, nodes = List(cn0, cn1, cn2, cn3, cn4), leaves = List(cn2, cn3, cn4))

    }

    it("nodes and leaves can be put into SGrpaph in unnatural order") {
      graph1a = mkGraph(root = cn0, nodes = List(cn0, cn2, cn1), leaves = List(cn2, cn1))
      graph2a = mkGraph(root = cn0, nodes = List(cn0, cn1, cn2, cn3, cn4).reverse, leaves = List(cn2, cn3, cn4).reverse)
    }

  }

  describe("TNodes and TEdges are created from the bottom up") {
    val n2 = TNode[Int, Int](conf = 2, outs = List(), base = None, tPath = List(1))
    val n1 = TNode[Int, Int](conf = 1, outs = List(), base = None, tPath = List(0))
    val e2 = TEdge[Int, Int](n2, 2)
    val e1 = TEdge[Int, Int](n1, 1)
    val n0 = TNode[Int, Int](conf = 0, outs = List(e1, e2), base = None, tPath = List())

    it("TGraph is constructed by enumerating root and leaves") {
      tgraph1 = TGraph(root = n0, leaves = List(n1, n2))
    }

    it("TNodes can be queried by its path") {
      assert(tgraph1.get(List()) === n0)
      assert(tgraph1.get(List(0)) === n1)
      assert(tgraph1.get(List(1)) === n2)
    }
  }

  describe("Top subtrees cannot be reused in different TGraphs, so extra top subtrees are created") {
    val n4 = TNode[Int, Int](conf = 4, outs = List(), base = Some(List()), tPath = List(0, 1))
    val n3 = TNode[Int, Int](conf = 3, outs = List(), base = Some(List(0, 1)), tPath = List(0, 0))
    val e3 = TEdge[Int, Int](n3, 3)
    val e4 = TEdge[Int, Int](n4, 4)
    val n2 = TNode[Int, Int](conf = 2, outs = List(), base = None, tPath = List(1))
    val n1 = TNode[Int, Int](conf = 1, outs = List(e3, e4), base = None, tPath = List(0))
    val e2 = TEdge[Int, Int](n2, 2)
    val e1 = TEdge[Int, Int](n1, 1)
    val n0 = TNode[Int, Int](conf = 0, outs = List(e1, e2), base = None, tPath = List())
    TGraph(root = n0, leaves = List(n2, n3, n4))

    it("TGraph is constructed by enumerating root and leaves") {
      tgraph2 = TGraph(root = n0, leaves = List(n2, n3, n4))
    }

    it("TNodes can be queried by its path") {
      assert(tgraph2.get(List()) === n0)
      assert(tgraph2.get(List(0)) === n1)
      assert(tgraph2.get(List(1)) === n2)
      assert(tgraph2.get(List(0, 0)) === n3)
      assert(tgraph2.get(List(0, 1)) === n4)
    }
  }

  describe("Transformations") {
    it("transpose SGraphs into corresponding TGraphs") {
      assert(Transformations.transpose(graph1) === tgraph1)
      assert(Transformations.transpose(graph2) === tgraph2)
    }

    it("and canonize corresponding graphs: the order of nodes, edges and leave should be natural") {
      assert(Transformations.transpose(graph1a) === tgraph1)
      assert(Transformations.transpose(graph2a) === tgraph2)
    }
  }
}
