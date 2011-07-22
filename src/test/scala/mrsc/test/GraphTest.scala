package mrsc.test

import org.junit.{ Test, Ignore }
import org.junit.Assert._

import mrsc._

class GraphTest {
  // traverse 2 very simple cographs
  @Test
  def traverse(): Unit = {
    val cn0 = CoNode[Int, Int, Extra](conf = 0, extraInfo = null, in = null, base = None, coPath = List())
    
    val ce1 = CoEdge[Int, Int, Extra](cn0, 1)
    val cn1 = CoNode[Int, Int, Extra](conf = 1, extraInfo = null, in = ce1, base = None, coPath = List(0))
    val ce2 = CoEdge[Int, Int, Extra](cn0, 2)
    val cn2 = CoNode[Int, Int, Extra](conf = 2, extraInfo = null, in = ce2, base = None, coPath = List(1))
    
    val ce3 = CoEdge[Int, Int, Extra](cn1, 3)
    val cn3 = CoNode[Int, Int, Extra](conf = 3, extraInfo = null, in = ce3, base = Some(List(1, 0)), coPath = List(0, 0))
    val ce4 = CoEdge[Int, Int, Extra](cn1, 4)
    val cn4 = CoNode[Int, Int, Extra](conf = 4, extraInfo = null, in = ce4, base = Some(List()), coPath = List(1, 0))
  
    val cograph1 = CoGraph(root = cn0, nodes = List(cn0, cn1, cn2), leaves = List(cn1, cn2))
    val cograph1a = CoGraph(root = cn0, nodes = List(cn0, cn2, cn1), leaves = List(cn2, cn1))
    val cograph2 = CoGraph(root = cn0, nodes = List(cn0, cn1, cn2, cn3, cn4), leaves = List(cn2, cn3, cn4))
    val cograph2a = CoGraph(root = cn0, nodes = List(cn0, cn1, cn2, cn3, cn4).reverse, leaves = List(cn2, cn3, cn4).reverse)
    
    val graph1 = Transformations.transpose(cograph1)
    val graph1a = Transformations.transpose(cograph1)
    val graph2 = Transformations.transpose(cograph2)
    val graph2a = Transformations.transpose(cograph2)
    
    println(cograph1)
    println(cograph2)
    
    println(graph1)
    println(graph1a)
    println(graph2)
    println(graph2a)
    println(graph2a.leaves)
    
    // The order of nodes and leaves in final graph should not depend
    // on the order of nodes and leaves in the initial cograph
    
    assertEquals(graph1, graph1a)
    assertEquals(graph2, graph2a)
    
    val n4 = Node[Int, Int, Extra](conf = 4, extraInfo = null, outs = List(), base = Some(List()), path = List(0, 1))
    val n3 = Node[Int, Int, Extra](conf = 3, extraInfo = null, outs = List(), base = Some(List(0, 1)), path = List(0, 0))
    val e3 = Edge[Int, Int, Extra](n3, 3)
    val e4 = Edge[Int, Int, Extra](n4, 4)
    
    val n2 = Node[Int, Int, Extra](conf = 2, extraInfo = null, outs = List(), base = None, path = List(1))
    val n1 = Node[Int, Int, Extra](conf = 1, extraInfo = null, outs = List(e3, e4), base = None, path = List(0))
    
    val e2 = Edge[Int, Int, Extra](n2, 2)
    val e1 = Edge[Int, Int, Extra](n1, 1)
    
    val n0 = Node[Int, Int, Extra](conf = 0, extraInfo = null, outs = List(e1, e2), base = None, path = List())
    n0.coPath
    
    // note that leaves in the breadth-first order
    val expectedGraph2 = Graph(root = n0, leaves = List(n2, n3, n4))
    
    println()
    println()
    println("expected")
    println(expectedGraph2)
    println("real")
    println(graph2)
    
    
    assertEquals(expectedGraph2.root, graph2.root)
  }
}