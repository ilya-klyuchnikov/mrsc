package mrsc.test

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import mrsc.core._
import mrsc.pfp._

@RunWith(classOf[JUnitRunner])
class BuilderCommandSpec extends mutable.Specification {
  args(sequential = true)

  var pg1, pg2: PartialCoGraph[Int, String, Extra[String]] = _
  var cg1, cg2: CoGraph[Int, String, Extra[String]] = _

  val graph: Graph[Int, String, Extra[String]] = {
    val n1 = Node[Int, String, Extra[String]](conf = 11, extraInfo = NoExtra, outs = List(), base = Some(List()), path = List(0))
    val e1 = Edge[Int, String, Extra[String]](n1, "-1 -> 11")
    val n0 = Node[Int, String, Extra[String]](conf = -1, extraInfo = NoExtra, outs = List(e1), base = None, path = List())
    Graph(root = n0, leaves = List(n1))
  }

  def start(c: Int): PartialCoGraph[Int,String,Extra[String]] = {
    val startNode = CoNode[Int, String, Extra[String]](c, NoExtra, null, None, Nil)
    new PartialCoGraph(List(startNode), Nil, Nil)
  }

  
  "The builder of the 1st cograph" in {
    "starts with a single-node cograph" in {
      pg1 = start(0)

      
      (pg1.current.conf must_== 0) and
        (pg1.complete.size must_== 0) and
        (pg1.incompleteLeaves.size must_== 1)
    }

    "executes AddChildNodes command" in {
      pg1 = pg1.addChildNodes(List((1, "0 -> 1", NoExtra), (2, "0 -> 2", NoExtra)))

      (pg1.current.conf must_== 1) and
        (pg1.complete.size must_== 1) and
        (pg1.incompleteLeaves.size must_== 2)
    }

    "executes ConvertToLeaf command" in {
      pg1 = pg1.convertToLeaf()

      (pg1.current.conf must_== 2) and
        (pg1.complete.size must_== 2) and
        (pg1.incompleteLeaves.size must_== 1)
    }

    "executes ReplaceNode command" in {
      val oldActive = pg1.current
      pg1 = pg1.rebuild(21, NoExtra)
      val newActive = pg1.current

      (newActive.conf must_== 21) and
        (newActive.in must_== oldActive.in) and
        (pg1.complete.size must_== 2) and
        (pg1.incompleteLeaves.size must_== 1)
    }

    "executes RollbackSubGraph command" in {
      val root = pg1.current.in.coNode
      pg1 = pg1.rollback(root, -1, NoExtra)

      (pg1.current.conf must_== -1) and
        (pg1.complete.size must_== 0) and
        (pg1.incompleteLeaves.size must_== 1)
    }

    "executes AddChildNodes command" in {
      pg1 = pg1.addChildNodes(List((11, "-1 -> 11", NoExtra)))

      (pg1.current.conf must_== 11) and
        (pg1.complete.size must_== 1) and
        (pg1.incompleteLeaves.size must_== 1)
    }

    "executes Fold command" in {
      pg1 = pg1.fold(List())

      (pg1.current must_== null) and
        (pg1.complete.size must_== 2) and
        (pg1.incompleteLeaves.size must_== 0)
    }

    "produces final cograph" in {
      cg1 = pg1.toCoGraph()
      (cg1.leaves.size must_== 1) and
        (cg1.root.conf must_== -1)
    }
  }

  "The builder of the 2nd cograph" in {
    "starts with a single-node cograph" in {
      pg2 = start(-1)

      (pg2.current.conf must_== -1) and
        (pg2.complete.size must_== 0) and
        (pg2.incompleteLeaves.size must_== 1)
    }

    "executes AddChildNodes command" in {
      pg2 = pg2.addChildNodes(List((11, "-1 -> 11", NoExtra)))

      (pg2.current.conf must_== 11) and
        (pg2.complete.size must_== 1) and
        (pg2.incompleteLeaves.size must_== 1)
    }

    "executes Fold command" in {
      pg2 = pg2.fold(List())

      (pg1.current must_== null) and
        (pg1.complete.size must_== 2) and
        (pg1.incompleteLeaves.size must_== 0)
    }

    "produces final cograph" in {
      cg2 = pg2.toCoGraph()
      (cg2.leaves.size must_== 1) and
        (cg2.root.conf must_== -1)
    }
  }

  "Produced cographs" should {
    "be equal to each other" in {
      cg1 must_== cg2
    }
    "be equal to sample graph" in {
      val g1 = Transformations.transpose(cg1)
      val g2 = Transformations.transpose(cg2)

      (g1 must_== graph) and (g2 must_== graph)
    }
  }
}