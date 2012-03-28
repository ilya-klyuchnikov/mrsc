package mrsc.pfp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import mrsc.pfp._

class ParserSuite extends FunSuite {

  test("parsing simple lambda abstraction #1") {
    val in = """\x -> x"""
    val expected = Abs(BVar(0))
    val parsed = PFPParsers.inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing simple lambda abstraction #2") {
    val in = """\x -> x x"""
    val expected = Abs(App(BVar(0), BVar(0)))
    val parsed = PFPParsers.inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing application #1") {
    val in = """(\x -> x) (\x -> x)"""
    val expected = App(Abs(BVar(0)), Abs(BVar(0)))
    val parsed = PFPParsers.inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing constructor #1") {
    val in = """Nil[]"""
    val expected = Ctr("Nil", List())
    val parsed = PFPParsers.inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing constructor #2") {
    val in = """Wrap[w:Nil[]]"""
    val expected = Ctr("Wrap", List(("w", Ctr("Nil", List()))))
    val parsed = PFPParsers.inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing case expression") {
    val in = """case Nil[] of { x: Nil -> x ; y: Cons -> y } """
    val expected = Case(Ctr("Nil", List()), List(("Nil", BVar(0)), ("Cons", BVar(0))))
    val parsed = PFPParsers.inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing deconstructon") {
    val in = """\x -> x.head"""
    val expected = Abs(DeCtr(BVar(0), "head"))
    val parsed = PFPParsers.inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }
}