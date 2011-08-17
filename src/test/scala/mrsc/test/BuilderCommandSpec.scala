package mrsc.test

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import mrsc.core._
import mrsc.pfp._

@RunWith(classOf[JUnitRunner])
class BuilderCommandSpec extends mutable.Specification {
  args(sequential = true)

  var pg1, pg2: Graph[Int, String, Extra[String]] = _
//  var cg1, cg2: TDGraph[Int, String, Extra[String]] = _

  val graph: TGraph[Int, String, Extra[String]] = {
    val n1 = TNode[Int, String, Extra[String]](conf = 11, extraInfo = NoExtra, outs = List(), back = Some(List()), tPath = List(0))
    val e1 = TEdge[Int, String, Extra[String]](n1, "-1 -> 11")
    val n0 = TNode[Int, String, Extra[String]](conf = -1, extraInfo = NoExtra, outs = List(e1), back = None, tPath = List())
    TGraph(root = n0, leaves = List(n1))
  }

  def start(c: Int): Graph[Int,String,Extra[String]] = {
    val startNode = Node[Int, String, Extra[String]](c, NoExtra, null, None, Nil)
    new Graph(List(startNode), Nil, Nil)
  }

  
  "The builder of the 1st graph" in {
    "starts with a single-node graph" in {
      pg1 = start(0)

      
      (pg1.current.conf must_== 0) and
        (pg1.completeNodes.size must_== 0) and
        (pg1.incompleteLeaves.size must_== 1)
    }

    "executes AddChildNodes command" in {
      pg1 = pg1.addChildNodes(List((1, "0 -> 1", NoExtra), (2, "0 -> 2", NoExtra)))

      (pg1.current.conf must_== 1) and
        (pg1.completeNodes.size must_== 1) and
        (pg1.incompleteLeaves.size must_== 2)
    }

    "executes ConvertToLeaf command" in {
      pg1 = pg1.convertToLeaf()

      (pg1.current.conf must_== 2) and
        (pg1.completeNodes.size must_== 2) and
        (pg1.incompleteLeaves.size must_== 1)
    }

    "executes ReplaceNode command" in {
      val oldActive = pg1.current
      pg1 = pg1.rebuild(21, NoExtra)
      val newActive = pg1.current

      (newActive.conf must_== 21) and
        (newActive.in must_== oldActive.in) and
        (pg1.completeNodes.size must_== 2) and
        (pg1.incompleteLeaves.size must_== 1)
    }

    "executes RollbackSubGraph command" in {
      val root = pg1.current.in.node
      pg1 = pg1.rollback(root, -1, NoExtra)

      (pg1.current.conf must_== -1) and
        (pg1.completeNodes.size must_== 0) and
        (pg1.incompleteLeaves.size must_== 1)
    }

    "executes AddChildNodes command" in {
      pg1 = pg1.addChildNodes(List((11, "-1 -> 11", NoExtra)))

      (pg1.current.conf must_== 11) and
        (pg1.completeNodes.size must_== 1) and
        (pg1.incompleteLeaves.size must_== 1)
    }

    "executes Fold command" in {
      pg1 = pg1.fold(List())

      (pg1.current must_== null) and
        (pg1.completeNodes.size must_== 2) and
        (pg1.incompleteLeaves.size must_== 0)
    }

//    "produces final graph" in {
//      cg1 = pg1
//      (cg1.leaves.size must_== 1) and
//        (cg1.root.conf must_== -1)
//    }
  }

  "The builder of the 2nd graph" in {
    "starts with a single-node graph" in {
      pg2 = start(-1)

      (pg2.current.conf must_== -1) and
        (pg2.completeNodes.size must_== 0) and
        (pg2.incompleteLeaves.size must_== 1)
    }

    "executes AddChildNodes command" in {
      pg2 = pg2.addChildNodes(List((11, "-1 -> 11", NoExtra)))

      (pg2.current.conf must_== 11) and
        (pg2.completeNodes.size must_== 1) and
        (pg2.incompleteLeaves.size must_== 1)
    }

    "executes Fold command" in {
      pg2 = pg2.fold(List())

      (pg1.current must_== null) and
        (pg1.completeNodes.size must_== 2) and
        (pg1.incompleteLeaves.size must_== 0)
    }

//    "produces final graph" in {
//      cg2 = pg2
//      (cg2.leaves.size must_== 1) and
//        (cg2.root.conf must_== -1)
//    }
  }

  "Produced graphs" should {
    "be equal to each other" in {
      pg1 must_== pg2
    }
    "be equal to sample graph" in {
      val g1 = Transformations.transpose(pg1)
      val g2 = Transformations.transpose(pg2)

      (g1 must_== graph) and (g2 must_== graph)
    }
  }
}