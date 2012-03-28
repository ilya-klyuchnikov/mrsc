package mrsc.pfp.test

import org.scalatest.FunSuite

import mrsc.pfp._

class EvaluationSuite extends FunSuite {
  test("simple evaluation #1") {
    val bindingsIn =
      """
      app = \x -> \y ->
        case x of {
          x1:Nil  -> y;
          x1:Cons -> Cons[head: x1.head, tail: (app x1.tail y)]
        }; 
      """
    val bindings: GContext = PFPParsers().inputBindings(bindingsIn)
    val goal = PFPParsers().inputTerm("app Nil[] Nil[]")
    val expected = PFPParsers().inputTerm("Nil[]")
    info(goal.toString())
    info(expected.toString())
    val evaled = CBNEval.eval(goal, bindings)
    info(evaled.toString())
    assert(evaled === expected)
  }

  test("simple evaluation #2") {
    val bindingsIn =
      """
      app = \x -> \y ->
        case x of {
          x1:Nil  -> y;
          x1:Cons -> Cons[head: x1.head, tail: (app x1.tail y)]
        }; 
      """
    val bindings: GContext = PFPParsers().inputBindings(bindingsIn)
    val goal = PFPParsers().inputTerm("app Cons[head: A[], tail: Nil[]] Nil[]")
    val expected = PFPParsers().inputTerm("Cons[head: A[], tail: Nil[]]")
    info(goal.toString())
    info(expected.toString())
    val evaled = CBNEval.eval(goal, bindings)
    info(evaled.toString())
    assert(evaled === expected)
  }
  
}