package mrsc.test

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import mrsc.core._
import mrsc.pfp._

@RunWith(classOf[JUnitRunner])
class GraphSpec extends mutable.Specification {

  args(sequential = true)

  /*
  var cograph1, cograph1a, cograph2, cograph2a: CoGraph[Int, Int, Extra[String]] = _
  var graph1, graph2: Graph[Int, Int, Extra[String]] = _

  "Conodes and coedges are created from the top down" in {

    val cn0 = CoNode[Int, Int, Extra[String]](conf = 0, extraInfo = null, in = null, base = None, coPath = List())
    val ce1 = CoEdge[Int, Int, Extra[String]](cn0, 1)
    val cn1 = CoNode[Int, Int, Extra[String]](conf = 1, extraInfo = null, in = ce1, base = None, coPath = List(0))
    val ce2 = CoEdge[Int, Int, Extra[String]](cn0, 2)
    val cn2 = CoNode[Int, Int, Extra[String]](conf = 2, extraInfo = null, in = ce2, base = None, coPath = List(1))
    val ce3 = CoEdge[Int, Int, Extra[String]](cn1, 3)
    val cn3 = CoNode[Int, Int, Extra[String]](conf = 3, extraInfo = null, in = ce3, base = Some(List(1, 0)), coPath = List(0, 0))
    val ce4 = CoEdge[Int, Int, Extra[String]](cn1, 4)
    val cn4 = CoNode[Int, Int, Extra[String]](conf = 4, extraInfo = null, in = ce4, base = Some(List()), coPath = List(1, 0))

    "the top subtrees can be reused for constructing different co graphs" in {
      cograph1 = CoGraph(root = cn0, nodes = List(cn0, cn1, cn2), leaves = List(cn1, cn2))
      cograph2 = CoGraph(root = cn0, nodes = List(cn0, cn1, cn2, cn3, cn4), leaves = List(cn2, cn3, cn4))
      success
    }

    "nodes and leaves can be put into cogrpaph in unnatural order" in {
      cograph1a = CoGraph(root = cn0, nodes = List(cn0, cn2, cn1), leaves = List(cn2, cn1))
      cograph2a = CoGraph(root = cn0, nodes = List(cn0, cn1, cn2, cn3, cn4).reverse, leaves = List(cn2, cn3, cn4).reverse)
      success
    }

  }

  "Nodes and edges are created from the bottom up" in {
    val n2 = Node[Int, Int, Extra[String]](conf = 2, extraInfo = null, outs = List(), base = None, path = List(1))
    val n1 = Node[Int, Int, Extra[String]](conf = 1, extraInfo = null, outs = List(), base = None, path = List(0))
    val e2 = Edge[Int, Int, Extra[String]](n2, 2)
    val e1 = Edge[Int, Int, Extra[String]](n1, 1)
    val n0 = Node[Int, Int, Extra[String]](conf = 0, extraInfo = null, outs = List(e1, e2), base = None, path = List())

    "graph is contructed by enumerating root and leaves" in {
      graph1 = Graph(root = n0, leaves = List(n1, n2))
      success
    }

    "nodes can be queried by its path" in ({
      graph1.get(List()) must_== n0
    } and {
      graph1.get(List(0)) must_== n1
    } and {
      graph1.get(List(1)) must_== n2
    })
  }

  "Top subtrees cannnot be reused in different graphs, so extra top subtrees are created" in {
    val n4 = Node[Int, Int, Extra[String]](conf = 4, extraInfo = null, outs = List(), base = Some(List()), path = List(0, 1))
    val n3 = Node[Int, Int, Extra[String]](conf = 3, extraInfo = null, outs = List(), base = Some(List(0, 1)), path = List(0, 0))
    val e3 = Edge[Int, Int, Extra[String]](n3, 3)
    val e4 = Edge[Int, Int, Extra[String]](n4, 4)
    val n2 = Node[Int, Int, Extra[String]](conf = 2, extraInfo = null, outs = List(), base = None, path = List(1))
    val n1 = Node[Int, Int, Extra[String]](conf = 1, extraInfo = null, outs = List(e3, e4), base = None, path = List(0))
    val e2 = Edge[Int, Int, Extra[String]](n2, 2)
    val e1 = Edge[Int, Int, Extra[String]](n1, 1)
    val n0 = Node[Int, Int, Extra[String]](conf = 0, extraInfo = null, outs = List(e1, e2), base = None, path = List())
    Graph(root = n0, leaves = List(n2, n3, n4))

    "graph is contructed by enumerating root and leaves" in {
      graph2 = Graph(root = n0, leaves = List(n2, n3, n4))
      success
    }

    "nodes can be queried by its path" in ({
      graph2.get(List()) must_== n0
    } and {
      graph2.get(List(0)) must_== n1
    } and {
      graph2.get(List(1)) must_== n2
    } and {
      graph2.get(List(0, 0)) must_== n3
    } and {
      graph2.get(List(0, 1)) must_== n4
    })
  }
*/
  /*
  "Transformations" should {
    "transpose cographs into corresponding graphs" in ({
      Transformations.transpose(cograph1) must_== graph1
    } and {
      Transformations.transpose(cograph2) must_== graph2
    })
    
    "canonize corresponding graphs: the order of nodes, edges and leave should be natural" in ({
      Transformations.transpose(cograph1a) must_== graph1
    } and {
      Transformations.transpose(cograph2a) must_== graph2
    })
  }
  */
}