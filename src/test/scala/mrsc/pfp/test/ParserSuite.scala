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
    testTermParsing("""Nil[]""", Ctr("Nil", List()))
    testTermParsing("""Wrap[w:Nil[]]""", Ctr("Wrap", List(("w", Ctr("Nil", List())))))
  }

  test("parsing case expression") {
    testTermParsing("""case Nil[] of { x: Nil -> x ; y: Cons -> y }""",
      Case(Ctr("Nil", List()), List(("Nil", BVar(0)), ("Cons", BVar(0)))))
  }

  test("parsing deconstructors") {
    testTermParsing("""\x -> x.head""", Abs(DeCtr(BVar(0), "head")))
  }

  test("parsing lets") {
    testTermParsing("""let x = Nil[] in x""",
      Let(Ctr("Nil", List()), BVar(0)))
    testTermParsing("""let x = Nil[] in let y = x in x y""",
      Let(Ctr("Nil", List()), Let(BVar(0), App(BVar(1), BVar(0)))))
  }

  test("parsing fix") {
    testTermParsing("""fix (\x -> x)""", Fix(Abs(BVar(0))))
  }

  test("parsing letrec") {
    testTermParsing("""letrec x = x in x""", Let(Fix(Abs(BVar(0))), BVar(0)))
  }

  test("parsing bindings") {
    val in =
      """app = \x -> \y ->
      |    case x of {
      |      x1:Nil  -> y;
      |      x1:Cons -> Cons[head: x1.head, tail: (app x1.tail y)]
      |    };""".stripMargin
    val expected = Map("app" ->
      Abs(Abs(Case(BVar(1), List(
        ("Nil", BVar(1)),
        ("Cons", Ctr("Cons", List(("head", DeCtr(BVar(0), "head")), ("tail", App(App(GVar("app"), DeCtr(BVar(0), "tail")), BVar(1)))))))))))
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