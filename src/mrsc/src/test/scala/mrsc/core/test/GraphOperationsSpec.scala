package mrsc.core.test

import org.scalatest.Spec
import mrsc.core._

class GraphOperationsSpec extends Spec {

  var g1, g2: SGraph[Int, String] = _
  var tg1, tg2: TGraph[Int, String] = _

  val graph: TGraph[Int, String] = {
    val n1 = TNode[Int, String](conf = 11, outs = List(), base = Some(List()), tPath = List(0))
    val e1 = TEdge[Int, String](n1, "-1 -> 11")
    val n0 = TNode[Int, String](conf = -1, outs = List(e1), base = None, tPath = List())
    TGraph(root = n0, leaves = List(n1))
  }

  def start(c: Int): SGraph[Int, String] = {
    val startNode = SNode[Int, String](c, null, None, Nil)
    SGraph(List(startNode), Nil, Nil)
  }

  describe("The builder of the 1st graph") {
    it("starts with a single-node graph") {
      g1 = start(0)

      assert(g1.current.conf == 0)
      assert(g1.completeNodes.size === 0)
      (g1.incompleteLeaves.size === 1)
    }

    it("executes AddChildNodesStep #1") {
      g1 = GraphGenerator.executeStep(AddChildNodesStep(List((1, "0 -> 1"), (2, "0 -> 2"))), g1)

      assert(g1.current.conf === 1)
      assert(g1.completeNodes.size === 1)
      assert(g1.incompleteLeaves.size === 2)
    }

    it("executes CompleteCurrentNodeStep") {
      g1 = GraphGenerator.executeStep(CompleteCurrentNodeStep(), g1)

      assert(g1.current.conf === 2)
      assert(g1.completeNodes.size === 2)
      assert(g1.incompleteLeaves.size === 1)
    }

    it("executes RebuildStep") {
      val oldActive = g1.current
      g1 = GraphGenerator.executeStep(RebuildStep(21), g1)
      val newActive = g1.current

      assert(newActive.conf === 21)
      assert(newActive.in === oldActive.in)
      assert(g1.completeNodes.size === 2)
      assert(g1.incompleteLeaves.size === 1)
    }

    it("executes RollbackStep") {
      val root = g1.current.in.node
      g1 = GraphGenerator.executeStep(RollbackStep(root.sPath, -1), g1)

      assert(g1.current.conf === -1)
      assert(g1.completeNodes.size === 0)
      assert(g1.incompleteLeaves.size === 1)
    }

    it("executes AddChildNodesStep #2") {
      g1 = GraphGenerator.executeStep(AddChildNodesStep(List((11, "-1 -> 11"))), g1)

      assert(g1.current.conf === 11)
      assert(g1.completeNodes.size === 1)
      assert(g1.incompleteLeaves.size === 1)
    }

    it("executes FoldStep") {
      val root = g1.current.in.node
      g1 = GraphGenerator.executeStep(FoldStep(root.sPath), g1)

      assert(g1.current === null)
      assert(g1.completeNodes.size === 2)
      assert(g1.incompleteLeaves.size === 0)
    }
  }

  describe("The builder of the 2nd graph") {
    it("starts with a single-node graph") {
      g2 = start(-1)

      assert(g2.current.conf === -1)
      assert(g2.completeNodes.size === 0)
      assert(g2.incompleteLeaves.size === 1)
    }

    it("executes AddChildNodesStep") {
      g2 = GraphGenerator.executeStep(AddChildNodesStep(List((11, "-1 -> 11"))), g2)

      assert(g2.current.conf === 11)
      assert(g2.completeNodes.size === 1)
      assert(g2.incompleteLeaves.size === 1)
    }

    it("executes FoldStep") {
      val root = g2.current.in.node
      g2 = GraphGenerator.executeStep(FoldStep(root.sPath), g2)

      assert(g1.current === null)
      assert(g1.completeNodes.size === 2)
      assert(g1.incompleteLeaves.size === 0)
    }
  }

  describe("Produced graphs") {
    it("be equal to sample graph") {
      val tgraph1 = Transformations.transpose(g1)
      val tgraph2 = Transformations.transpose(g2)

      assert(tgraph1 === graph)
      assert(tgraph2 === graph)
    }
  }
}