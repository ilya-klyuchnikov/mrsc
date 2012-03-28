package mrsc.pfp.test

import org.scalatest.FunSuite

import mrsc.core._
import mrsc.pfp._

class DeforestationSuite extends FunSuite {

  test("deforest append") {
    val bindingsIn =
      """
      app = \x -> \y ->
        case x of {
          x1:Nil  -> y;
          x1:Cons -> Cons[head: x1.head, tail: (app x1.tail y)]
        }; 
      """
    val bindings: GContext = PFPParsers().inputBindings(bindingsIn)
    val goal: Term = "app <x> <y>"
    val rules = new Deforester(bindings)
    val graphs = GraphGenerator(rules, goal)
    for (g <- graphs) {
      val tg = Transformations.transpose(g)
      println(tg)
    }
  }
  
  test("deforest double append") {
    val bindingsIn =
      """
      app = \x -> \y ->
        case x of {
          x1:Nil  -> y;
          x1:Cons -> Cons[head: x1.head, tail: (app x1.tail y)]
        }; 
      """
    val bindings: GContext = PFPParsers().inputBindings(bindingsIn)
    val goal: Term = "app (app <x> <y>) <z> "
    val rules = new Deforester(bindings)
    val graphs = GraphGenerator(rules, goal)
    for (g <- graphs) {
      val tg = Transformations.transpose(g)
      println()
      println(tg)
    }
  }
}