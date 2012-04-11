package mrsc.pfp.test

import org.scalatest.FunSuite

import mrsc.core._
import mrsc.pfp._

class DeforestationSuite extends FunSuite {

  test("build graph for append") {
    val bindings: GContext =
      """
      app = \x -> \y ->
        case x of {
          x1:Nil  -> y;
          x1:Cons -> Cons[head: x1.head, tail: (app x1.tail y)]
        }; 
      """
    val goal: Term = "app <x> <y>"
    val rules = new Deforester(bindings)
    val graphs = GraphGenerator(rules, goal)
    for (g <- graphs) {
      val tg = Transformations.transpose(g)
      println(tg)
    }
  }

  test("build graph for double append") {
    val bindings: GContext =
      """
      app = \x -> \y ->
        case x of {
          x1:Nil  -> y;
          x1:Cons -> Cons[head: x1.head, tail: (app x1.tail y)]
        }; 
      """
    val goal: Term = "app (app <x> <y>) <z> "
    val rules = new Deforester(bindings)
    val graphs = GraphGenerator(rules, goal)
    for (g <- graphs) {
      val tg = Transformations.transpose(g)
      println()
      println(tg)
    }
  }

  test("constructor residuation") {
    val bindings: GContext = ""
    val goal: Term = "Nil[]"
    val expectedResult: Term = "Nil[]"
    testExample(bindings, goal, expectedResult)
  }

  test("variable residuation") {
    val bindings: GContext = ""
    val goal: Term = "<x>"
    val expectedResult: Term = "<x>"
    testExample(bindings, goal, expectedResult)
  }

  test("constructor with variables residuation") {
    val bindings: GContext = ""
    val goal: Term = "Cons[head: <x>, tail: <y>]"
    val expectedResult: Term = goal
    testExample(bindings, goal, expectedResult)
  }

  test("naive folding") {
    val bindings: GContext = """f = \x -> f x;"""
    val rules = new Deforester(bindings)
    val goal: Term = "f <y>"

    val expectedResult: Term = """letrec h = \x -> h x in h <y>"""
    val graphs = GraphGenerator(rules, goal).toList
    val tGraph = Transformations.transpose(graphs.head)
    info(tGraph.toString())
    val result: Term = Residuator(tGraph).result
    info(result.toString())
    assert(expectedResult === result)
  }

  test("append residuation") {
    val bindings: GContext =
      """
      app = \x -> \y ->
        case x of {
          x1:Nil  -> y;
          x1:Cons -> Cons[head: x1.head, tail: (app x1.tail y)]
        }; 
      """
    val rules = new Deforester(bindings)
    val goal: Term = "app <x> <y>"

    val expectedResult: Term = 
      """
      letrec h = \x -> \y ->
        case x of {
          x1:Nil  -> y;
          x1:Cons -> Cons[head: x.head, tail: (h x.tail y)]
        } in h <x> <y>
      """
    val graphs = GraphGenerator(rules, goal).toList
    val tGraph = Transformations.transpose(graphs.head)
    info(tGraph.toString())
    val result: Term = Residuator(tGraph).result
    info(result.toString())
    assert(expectedResult === result)
  }

  def testExample(bindings: GContext, goal: Term, expectedResult: Term): Unit = {
    val rules = new Deforester(bindings)
    val graphs = GraphGenerator(rules, goal).toList
    val tGraph = Transformations.transpose(graphs.head)
    info(tGraph.toString())
    val result = Residuator(tGraph).result
    info(result.toString())
    assert(result === expectedResult)
  }
}