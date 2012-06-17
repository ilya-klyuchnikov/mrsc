package mrsc.pfp.test

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import mrsc.core._
import mrsc.pfp._

case class Deforester(val gc: GContext) extends PFPRules
  with PFPSyntax
  with PFPSemantics
  with Driving
  with Folding
  with NoWhistle
  with NoRebuildings

@RunWith(classOf[JUnitRunner])
class DeforestationSuite extends FunSuite {

  test("build graph for append") {
    val bindings: GContext =
      """
      app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, (app xs y))
        }; 
      """
    val goal: Term = "app <1> <2>"
    val rules = new Deforester(bindings)
    val graphs = GraphGenerator(rules, goal)
    for (g <- graphs) {
      val tg = Transformations.transpose(g)
      info(tg.toString)
    }
  }

  test("build graph for double append") {
    val bindings: GContext =
      """
      app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, (app xs y))
        }; 
      """
    val goal: Term = "app (app <1> <2>) <3> "
    val rules = new Deforester(bindings)
    val graphs = GraphGenerator(rules, goal)
    for (g <- graphs) {
      val tg = Transformations.transpose(g)
      info("")
      info(tg.toString)
    }
  }

  test("constructor residuation") {
    val bindings: GContext = ""
    val goal: Term = "Nil()"
    val expectedResult: Term = "Nil()"
    testExample(bindings, goal, expectedResult)
  }

  test("variable residuation") {
    val bindings: GContext = ""
    val goal: Term = "<1>"
    val expectedResult: Term = "<1>"
    testExample(bindings, goal, expectedResult)
  }

  test("constructor with variables residuation") {
    val bindings: GContext = ""
    val goal: Term = "Cons(<1>, <2>)"
    val expectedResult: Term = goal
    testExample(bindings, goal, expectedResult)
  }

  test("naive folding") {
    val bindings: GContext = """f = \x -> f x;"""
    val rules = new Deforester(bindings)
    val goal: Term = "f <2>"

    val expectedResult: Term = """letrec h = \x -> h x in h <2>"""
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
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, (app xs y))
        }; 
      """
    val rules = new Deforester(bindings)
    val goal: Term = "app <1> <2>"

    val expectedResult: Term =
      """
      letrec h = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, h xs y)
        } in h <1> <2>
      """
    val graphs = GraphGenerator(rules, goal).toList
    val tGraph = Transformations.transpose(graphs.head)
    info(tGraph.toString())
    val result: Term = Residuator(tGraph).result
    info(result.toString())
    assert(expectedResult === result)
  }

  test("append double deforestation") {
    val bindings: GContext =
      """
      """
    val rules = new Deforester(bindings)
    val goal: Term =
      """
      letrec h = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, h xs y)
        } in h <1> <2>
      """

    val expectedResult: Term =
      """
      letrec h = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, h xs y)
        } in h <1> <2>
      """
    val graphs = GraphGenerator(rules, goal).toList
    val tGraph = Transformations.transpose(graphs.head)
    info(tGraph.toString())
    val result: Term = Residuator(tGraph).result
    info(result.toString())
    assert(expectedResult === result)
  }

  test("double append residuation") {
    val bindings: GContext =
      """
      app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, (app xs y))
        }; 
      """
    val rules = new Deforester(bindings)
    val goal: Term = "app (app <1> <2>) <3>"

    val expectedResult: Term =
      """
      letrec h0 = \x -> \y -> \z ->
        case x of {
          Nil()  -> 
              letrec h1 = \x -> \y ->
                case x of {
                  Nil()  -> y;
                  Cons(x, xs) -> Cons(x, h1 xs y)
                } in h1 y z;
          Cons(x, xs) -> Cons(x, h0 xs y z)
        } in h0 <1> <2> <3>
      """
    val graphs = GraphGenerator(rules, goal).toList
    val tGraph = Transformations.transpose(graphs.head)
    info(tGraph.toString())
    val result: Term = Residuator(tGraph).result
    info(result.toString())
    assert(expectedResult === result)
  }

  test("flip residuation") {
    val bindings: GContext =
      """
      flip = \zt -> case zt of {
        Leaf(z)   -> Leaf(z);
        Branch(xt, yt) -> Branch(flip yt, flip xt)
      };
      """
    val rules = new Deforester(bindings)
    val goal: Term = "flip (flip <1>)"

    val expectedResult: Term =
      """
      letrec h1 = \zt -> case zt of {
        Leaf(z)   -> Leaf(z);
        Branch(xt, yt) -> Branch(h1 xt, h1 yt)
      } in h1 <1>
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
