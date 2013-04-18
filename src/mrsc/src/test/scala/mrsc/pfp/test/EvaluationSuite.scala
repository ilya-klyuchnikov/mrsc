package mrsc.pfp.test

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import mrsc.pfp._

@RunWith(classOf[JUnitRunner])
class EvaluationSuite extends FunSuite {
  val bindings = io.bingingsFromFile("pfp/defs/list.pfp")

  test("simple evaluation #1") {
    val goal = PFPParsers().inputTerm("app Nil() Nil()")
    val expected = PFPParsers().inputTerm("Nil()")
    info(goal.toString())
    info(expected.toString())
    val evaled = CBNEval.eval(goal, bindings)
    info(evaled.toString())
    assert(evaled === expected)
  }

  test("simple evaluation #2") {
    val goal = PFPParsers().inputTerm("app Cons(A(), Nil()) Nil()")
    val expected = PFPParsers().inputTerm("Cons(A(), Nil())")
    info(goal.toString())
    info(expected.toString())
    val evaled = CBNEval.eval(goal, bindings)
    info(evaled.toString())
    assert(evaled === expected)
  }
  
  test("letrec evaluation #1") {
    val goalIn =
      """
      letrec app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, (app xs y))
        } in app Cons(A(), Nil()) Nil()
      """
    val goal = PFPParsers().inputTerm(goalIn)
    val expected = PFPParsers().inputTerm("Cons(A(), Nil())")
    val evaled = CBNEval.eval(goal, Map())
    info(goalIn + " => " + evaled)
    assert(evaled === expected)
  }
  
}