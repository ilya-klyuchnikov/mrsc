package mrsc.test

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import mrsc.core._
import mrsc.pfp._

@RunWith(classOf[JUnitRunner])
class GraphOperationsSpec extends mutable.Specification
  with MachineSteps[Int, String] {
  
  args(sequential = true)
  
  var g1, g2: Graph[Int, String] = _
  var tg1, tg2: TGraph[Int, String] = _

  val graph: TGraph[Int, String] = {
    val n1 = TNode[Int, String](conf = 11, outs = List(), back = Some(List()), tPath = List(0))
    val e1 = TEdge[Int, String](n1, "-1 -> 11")
    val n0 = TNode[Int, String](conf = -1, outs = List(e1), back = None, tPath = List())
    TGraph(root = n0, leaves = List(n1))
  }

  def start(c: Int): Graph[Int,String] = {
    val startNode = Node[Int, String](c, null, None, Nil)
    Graph(List(startNode), Nil, Nil)
  }

  
  "The builder of the 1st graph" in {
    "starts with a single-node graph" in {
      g1 = start(0)

      
      (g1.current.conf must_== 0) and
        (g1.completeNodes.size must_== 0) and
        (g1.incompleteLeaves.size must_== 1)
    }

    "executes AddChildNodes command" in {
      g1 = addChildNodes(List((1, "0 -> 1"), (2, "0 -> 2")))(g1)

      (g1.current.conf must_== 1) and
        (g1.completeNodes.size must_== 1) and
        (g1.incompleteLeaves.size must_== 2)
    }

    "executes CompleteCurrentNode command" in {
      g1 = completeCurrentNode(g1)

      (g1.current.conf must_== 2) and
        (g1.completeNodes.size must_== 2) and
        (g1.incompleteLeaves.size must_== 1)
    }

    "executes ReplaceNode command" in {
      val oldActive = g1.current
      g1 = rebuild(21)(g1)
      val newActive = g1.current

      (newActive.conf must_== 21) and
        (newActive.in must_== oldActive.in) and
        (g1.completeNodes.size must_== 2) and
        (g1.incompleteLeaves.size must_== 1)
    }

    "executes RollbackSubGraph command" in {
      val root = g1.current.in.node
      g1 = rollback(root, -1)(g1)

      (g1.current.conf must_== -1) and
        (g1.completeNodes.size must_== 0) and
        (g1.incompleteLeaves.size must_== 1)
    }

    "executes AddChildNodes command" in {
      g1 = addChildNodes(List((11, "-1 -> 11")))(g1)

      (g1.current.conf must_== 11) and
        (g1.completeNodes.size must_== 1) and
        (g1.incompleteLeaves.size must_== 1)
    }

    "executes Fold command" in {
      val root = g1.current.in.node
      g1 = fold(root)(g1)

      (g1.current must_== null) and
        (g1.completeNodes.size must_== 2) and
        (g1.incompleteLeaves.size must_== 0)
    }
  }

  "The builder of the 2nd graph" in {
    "starts with a single-node graph" in {
      g2 = start(-1)

      (g2.current.conf must_== -1) and
        (g2.completeNodes.size must_== 0) and
        (g2.incompleteLeaves.size must_== 1)
    }

    "executes AddChildNodes command" in {
      g2 = addChildNodes(List((11, "-1 -> 11")))(g2)

      (g2.current.conf must_== 11) and
        (g2.completeNodes.size must_== 1) and
        (g2.incompleteLeaves.size must_== 1)
    }

    "executes Fold command" in {
      val root = g2.current.in.node
      g2 = fold(root)(g2)

      (g1.current must_== null) and
        (g1.completeNodes.size must_== 2) and
        (g1.incompleteLeaves.size must_== 0)
    }
  }

  "Produced graphs" should {
    "be equal to each other" in {
      g1 must_== g2
    }
    "be equal to sample graph" in {
      val tgraph1 = Transformations.transpose(g1)
      val tgraph2 = Transformations.transpose(g2)

      (tgraph1 must_== graph) and (tgraph2 must_== graph)
    }
  }
}