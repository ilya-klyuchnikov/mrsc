package mrsc.test

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import mrsc.core._
import mrsc.pfp._

object GraphSpec extends mutable.Specification {

  def mkGraph(root: Node[Int, Int] = null,
      nodes: List[Node[Int, Int]] = List(),
      leaves: List[Node[Int, Int]] = List()) = {
    Graph(List(), leaves, nodes)
  }  
}

@RunWith(classOf[JUnitRunner])
class GraphSpec extends mutable.Specification {
  import mrsc.test.GraphSpec.mkGraph
  
  args(sequential = true)

  var graph1, graph1a, graph2, graph2a: Graph[Int, Int] = _
  var tgraph1, tgraph2: TGraph[Int, Int] = _

  "Conodes and coedges are created from the top down" in {

    val cn0 = Node[Int, Int](conf = 0, in = null, back = None, path = List())
    val ce1 = Edge[Int, Int](cn0, 1)
    val cn1 = Node[Int, Int](conf = 1, in = ce1, back = None, path = List(0))
    val ce2 = Edge[Int, Int](cn0, 2)
    val cn2 = Node[Int, Int](conf = 2, in = ce2, back = None, path = List(1))
    val ce3 = Edge[Int, Int](cn1, 3)
    val cn3 = Node[Int, Int](conf = 3, in = ce3, back = Some(List(1, 0)), path = List(0, 0))
    val ce4 = Edge[Int, Int](cn1, 4)
    val cn4 = Node[Int, Int](conf = 4, in = ce4, back = Some(List()), path = List(1, 0))

    "the top subtrees can be reused for constructing different co graphs" in {
      graph1 = mkGraph(root = cn0, nodes = List(cn0, cn1, cn2), leaves = List(cn1, cn2))
      graph2 = mkGraph(root = cn0, nodes = List(cn0, cn1, cn2, cn3, cn4), leaves = List(cn2, cn3, cn4))
      success
    }

    "nodes and leaves can be put into cogrpaph in unnatural order" in {
      graph1a = mkGraph(root = cn0, nodes = List(cn0, cn2, cn1), leaves = List(cn2, cn1))
      graph2a = mkGraph(root = cn0, nodes = List(cn0, cn1, cn2, cn3, cn4).reverse, leaves = List(cn2, cn3, cn4).reverse)
      success
    }

  }

  "Nodes and edges are created from the bottom up" in {
    val n2 = TNode[Int, Int](conf = 2, outs = List(), back = None, tPath = List(1))
    val n1 = TNode[Int, Int](conf = 1, outs = List(), back = None, tPath = List(0))
    val e2 = TEdge[Int, Int](n2, 2)
    val e1 = TEdge[Int, Int](n1, 1)
    val n0 = TNode[Int, Int](conf = 0, outs = List(e1, e2), back = None, tPath = List())

    "graph is contructed by enumerating root and leaves" in {
      tgraph1 = TGraph(root = n0, leaves = List(n1, n2))
      success
    }

    "nodes can be queried by its path" in ({
      tgraph1.get(List()) must_== n0
    } and {
      tgraph1.get(List(0)) must_== n1
    } and {
      tgraph1.get(List(1)) must_== n2
    })
  }

  "Top subtrees cannnot be reused in different graphs, so extra top subtrees are created" in {
    val n4 = TNode[Int, Int](conf = 4, outs = List(), back = Some(List()), tPath = List(0, 1))
    val n3 = TNode[Int, Int](conf = 3, outs = List(), back = Some(List(0, 1)), tPath = List(0, 0))
    val e3 = TEdge[Int, Int](n3, 3)
    val e4 = TEdge[Int, Int](n4, 4)
    val n2 = TNode[Int, Int](conf = 2, outs = List(), back = None, tPath = List(1))
    val n1 = TNode[Int, Int](conf = 1, outs = List(e3, e4), back = None, tPath = List(0))
    val e2 = TEdge[Int, Int](n2, 2)
    val e1 = TEdge[Int, Int](n1, 1)
    val n0 = TNode[Int, Int](conf = 0, outs = List(e1, e2), back = None, tPath = List())
    TGraph(root = n0, leaves = List(n2, n3, n4))

    "graph is contructed by enumerating root and leaves" in {
      tgraph2 = TGraph(root = n0, leaves = List(n2, n3, n4))
      success
    }

    "nodes can be queried by its path" in ({
      tgraph2.get(List()) must_== n0
    } and {
      tgraph2.get(List(0)) must_== n1
    } and {
      tgraph2.get(List(1)) must_== n2
    } and {
      tgraph2.get(List(0, 0)) must_== n3
    } and {
      tgraph2.get(List(0, 1)) must_== n4
    })
  }

  "Transformations" should {
    "transpose cographs into corresponding graphs" in ({
      Transformations.transpose(graph1) must_== tgraph1
    } and {
      Transformations.transpose(graph2) must_== tgraph2
    })
    
    "canonize corresponding graphs: the order of nodes, edges and leave should be natural" in ({
      Transformations.transpose(graph1a) must_== tgraph1
    } and {
      Transformations.transpose(graph2a) must_== tgraph2
    })
  }
}