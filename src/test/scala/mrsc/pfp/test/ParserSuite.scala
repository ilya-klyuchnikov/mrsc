package mrsc.pfp.test

import org.scalatest.FunSuite

import mrsc.pfp._

class ParserSuite extends FunSuite {

  test("parsing simple lambda abstraction #1") {
    val in = """\x -> x"""
    val expected = Abs(BVar(0))
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing simple lambda abstraction #2") {
    val in = """\x -> x x"""
    val expected = Abs(App(BVar(0), BVar(0)))
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing application #1") {
    val in = """(\x -> x) (\x -> x)"""
    val expected = App(Abs(BVar(0)), Abs(BVar(0)))
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing constructor #1") {
    val in = """Nil[]"""
    val expected = Ctr("Nil", List())
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing constructor #2") {
    val in = """Wrap[w:Nil[]]"""
    val expected = Ctr("Wrap", List(("w", Ctr("Nil", List()))))
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing case expression") {
    val in = """case Nil[] of { x: Nil -> x ; y: Cons -> y } """
    val expected = Case(Ctr("Nil", List()), List(("Nil", BVar(0)), ("Cons", BVar(0))))
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing deconstructon") {
    val in = """\x -> x.head"""
    val expected = Abs(DeCtr(BVar(0), "head"))
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing let # 1") {
    val in = """let x = Nil[] in x"""
    val expected = Let(Ctr("Nil", List()), BVar(0))
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing let # 2") {
    val in = """let x = Nil[] in let y = x in x y"""
    val expected = Let(Ctr("Nil", List()), Let(BVar(0), App(BVar(1), BVar(0))))
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing fix") {
    val in = """fix (\x -> x)"""
    val expected = Fix(Abs(BVar(0)))
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing letrec") {
    val in = """letrec x = x in x"""
    val expected = Let(Fix(Abs(BVar(0))), BVar(0))
    val parsed = PFPParsers().inputTerm(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }

  test("parsing bindings") {
    val in =
      """
      app = \x -> \y ->
        case x of {
          x1:Nil  -> y;
          x1:Cons -> Cons[head: x1.head, tail: (app x1.tail y)]
        }; 
      """
    val expected = Map("app" ->
      Abs(Abs(Case(BVar(1), List(("Nil", BVar(1)), ("Cons", Ctr("Cons", List(("head", DeCtr(BVar(0), "head")), ("tail", App(App(GVar("app"), DeCtr(BVar(0), "tail")), BVar(1)))))))))))
    val parsed = PFPParsers().inputBindings(in)
    info(in + " ==> " + parsed.toString())
    assert(parsed === expected)
  }
}