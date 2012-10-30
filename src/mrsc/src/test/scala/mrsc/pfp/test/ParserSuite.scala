package mrsc.pfp.test

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import mrsc.pfp._

@RunWith(classOf[JUnitRunner])
class ParserSuite extends FunSuite {

  test("parsing free variable") {
    testTermParsing("<1>", FVar(1))
  }

  test("parsing global variable") {
    testTermParsing("app", GVar("app"))
  }

  test("parsing simple lambda abstractions") {
    testTermParsing("""\x -> x""", Abs(BVar(0)))
    testTermParsing("""\x -> x x""", Abs(App(BVar(0), BVar(0))))
  }

  test("parsing applications") {
    testTermParsing("""(\x -> x) (\x -> x)""", App(Abs(BVar(0)), Abs(BVar(0))))
  }

  test("parsing constructors") {
    testTermParsing("""Nil()""", Ctr("Nil", List()))
    testTermParsing("""Wrap(Nil())""", Ctr("Wrap", List(Ctr("Nil", List()))))
  }

  test("parsing case expression") {
    testTermParsing("""case Nil() of { Nil() -> Nil() ; Cons(a, b) -> Cons(a, b) }""",
      Case(Ctr("Nil", List()), List((Ptr("Nil", List()), Ctr("Nil", List())), (Ptr("Cons", List("a", "b")), Ctr("Cons", List(BVar(1), BVar(0)))))))
  }
  
  test("parsing case expression inside lambda") {
    testTermParsing("""\x -> case x of {Cons(y, ys) -> x}""", Abs(Case(BVar(0), List((Ptr("Cons", List("y", "ys")), BVar(2))))))
  }

  test("parsing lets") {
    testTermParsing("""let x = Nil() in x""",
      Let(Ctr("Nil", List()), BVar(0)))
    testTermParsing("""let x = Nil() in let y = x in x y""",
      Let(Ctr("Nil", List()), Let(BVar(0), App(BVar(1), BVar(0)))))
  }

  test("parsing letrec") {
    testTermParsing("""letrec x = x in x""", Let(Fix(BVar(0)), BVar(0)))
  }

  test("parsing bindings") {
    val in =
      """app = \x -> \y ->
      |    case x of {
      |      Nil()  -> y;
      |      Cons(x, xs) -> Cons(x, (app xs y))
      |    };""".stripMargin
    val expected = Map("app" ->
      Abs(Abs(Case(BVar(1), List(
        (Ptr("Nil", List()), BVar(0)),
        ((Ptr("Cons", List("x", "xs")), Ctr("Cons", List(BVar(1), App(App(GVar("app"), BVar(0)), BVar(2)) )))))))))
    testBindingsParsing(in, expected)
  }

  private def testTermParsing(in: String, expected: Term): Unit = {
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  private def testBindingsParsing(in: String, expected: GContext): Unit = {
    val parsed = PFPParsers().inputBindings(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }
}