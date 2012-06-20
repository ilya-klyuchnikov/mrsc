package mrsc.pfp.test

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import mrsc.core._
import mrsc.pfp._

// only UpperMsg??
class ClassicSC(val gc: GContext) extends PFPRules
  with PFPSyntax
  with PFPSemantics
  with PositiveDriving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HEByCouplingWhistle
  with LowerMsgOrUpperMsgOnBinaryWhistle

@RunWith(classOf[JUnitRunner])
class SimpleSupercompilationSuite extends FunSuite {

  test("append residuation") {
    val bindings: GContext =
      """
      app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, (app xs y))
        }; 
      """
    val goal: Term = "app <1> <2>"

    val expectedResult: Term =
      """
      letrec h = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, h xs y)
        } in h <1> <2>
      """
    testExample(bindings, goal, expectedResult)
  }

  test("append with repeated var") {
    val bindings: GContext =
      """
      app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, (app xs y))
        }; 
      """
    val goal: Term = "app <1> <1>"

    val expectedResult: Term =
      """
      letrec h = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, h xs y)
        } in h <1> <1>
      """
    testExample(bindings, goal, expectedResult)
  }

  def testExample(bindings: GContext, goal: Term, expectedResult: Term): Unit = {
    val rules = new ClassicSC(bindings)
    val graphs = GraphGenerator(rules, goal).toList
    val tGraph = Transformations.transpose(graphs.head)
    info(tGraph.toString())
    val result = Residuator(tGraph).result
    info(result.toString())
    assert(result === expectedResult)
  }
}