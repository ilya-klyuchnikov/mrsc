package mrsc.pfp.test

import mrsc.pfp._

class ParserSuite extends org.scalatest.FunSuite {

  test("parsing free variable") {
    testTermParsing("<1>", FVar(1))
    testTermParsing("*<1>", FVar(1, 1))
    testTermParsing("**<1>", FVar(1, 2))
  }

  test("parsing global variable") {
    testTermParsing("app", GVar("app"))
    testTermParsing("*app", GVar("app", 1))
    testTermParsing("**app", GVar("app", 2))
  }

  test("parsing simple lambda abstractions") {
    testTermParsing("""\x -> x""", Abs(BVar(0)))
    testTermParsing("""\x -> *x""", Abs(BVar(0, 1)))
    testTermParsing("""*\x -> x""", Abs(BVar(0, 0), 1))
    testTermParsing("""*\x -> *x""", Abs(BVar(0, 1), 1))
    testTermParsing("""\x -> x x""", Abs(App(BVar(0), BVar(0))))

    testTermParsing("""\x -> *x x""", Abs(App(BVar(0, 1), BVar(0))))
    testTermParsing("""\x -> *x *x""", Abs(App(BVar(0, 1), BVar(0, 1))))
    testTermParsing("""\x -> (*x *x)""", Abs(App(BVar(0, 1), BVar(0, 1))))
    testTermParsing("""\x -> *(*x *x)""", Abs(App(BVar(0, 1), BVar(0, 1), 1)))
    testTermParsing("""*\x -> *(*x *x)""", Abs(App(BVar(0, 1), BVar(0, 1), 1), 1))
    testTermParsing("""*\x -> *(*(x) *(x))""", Abs(App(BVar(0, 1), BVar(0, 1), 1), 1))
  }

  test("parsing applications") {
    testTermParsing("""(\x -> x) (\x -> x)""", App(Abs(BVar(0)), Abs(BVar(0))))
    testTermParsing("""*((\x -> x) (\x -> x))""", App(Abs(BVar(0)), Abs(BVar(0)), 1))
    testTermParsing("""*((\x -> *x) (\x -> *x))""", App(Abs(BVar(0, 1)), Abs(BVar(0, 1)), 1))
  }

  test("parsing constructors") {
    testTermParsing("""Nil()""", Ctr("Nil", List()))
    testTermParsing("""*Nil()""", Ctr("Nil", List(), 1))
    testTermParsing("""Wrap(Nil())""", Ctr("Wrap", List(Ctr("Nil", List()))))
    testTermParsing("""Wrap(*Nil())""", Ctr("Wrap", List(Ctr("Nil", List(), 1))))
  }

  test("parsing case expression") {
    testTermParsing(
      """case Nil() of { Nil() -> Nil() ; Cons(a, b) -> Cons(a, b) }""",
      Case(
        Ctr("Nil", List()),
        List(
          (Ptr("Nil", List()), Ctr("Nil", List())),
          (Ptr("Cons", List("a", "b")), Ctr("Cons", List(BVar(1), BVar(0)))),
        ),
      ),
    )
    testTermParsing(
      """*case Nil() of { Nil() -> Nil() ; Cons(a, b) -> Cons(a, b) }""",
      Case(
        Ctr("Nil", List()),
        List(
          (Ptr("Nil", List()), Ctr("Nil", List())),
          (Ptr("Cons", List("a", "b")), Ctr("Cons", List(BVar(1), BVar(0)))),
        ),
        1,
      ),
    )
    testTermParsing(
      """*case Nil() of { Nil() -> *Nil() ; Cons(a, b) -> *Cons(a, b) }""",
      Case(
        Ctr("Nil", List()),
        List(
          (Ptr("Nil", List()), Ctr("Nil", List(), 1)),
          (Ptr("Cons", List("a", "b")), Ctr("Cons", List(BVar(1), BVar(0)), 1)),
        ),
        1,
      ),
    )
    testTermParsing(
      """*case *Nil() of { Nil() -> *Nil() ; Cons(a, b) -> *Cons(a, b) }""",
      Case(
        Ctr("Nil", List(), 1),
        List(
          (Ptr("Nil", List()), Ctr("Nil", List(), 1)),
          (Ptr("Cons", List("a", "b")), Ctr("Cons", List(BVar(1), BVar(0)), 1)),
        ),
        1,
      ),
    )
  }

  test("parsing case expression inside lambda") {
    testTermParsing(
      """\x -> case x of {Cons(y, ys) -> x}""",
      Abs(Case(BVar(0), List((Ptr("Cons", List("y", "ys")), BVar(2))))),
    )
    testTermParsing(
      """\x -> case *x of {Cons(y, ys) -> x}""",
      Abs(Case(BVar(0, 1), List((Ptr("Cons", List("y", "ys")), BVar(2))))),
    )
    testTermParsing(
      """*\x -> case *x of {Cons(y, ys) -> x}""",
      Abs(Case(BVar(0, 1), List((Ptr("Cons", List("y", "ys")), BVar(2)))), 1),
    )
  }

  test("parsing lets") {
    testTermParsing("""let x = Nil() in x""", Let(Ctr("Nil", List()), BVar(0)))
    testTermParsing("""let x = Nil() in *x""", Let(Ctr("Nil", List()), BVar(0, 1)))
    testTermParsing("""let x = **Nil() in *x""", Let(Ctr("Nil", List(), 2), BVar(0, 1)))
    testTermParsing(
      """let x = Nil() in let y = x in x y""",
      Let(Ctr("Nil", List()), Let(BVar(0), App(BVar(1), BVar(0)))),
    )
    testTermParsing(
      """let x = Nil() in let y = x in **(x y)""",
      Let(Ctr("Nil", List()), Let(BVar(0), App(BVar(1), BVar(0), 2))),
    )
  }

  test("parsing letrec") {
    testTermParsing("""letrec x = x in x""", Let(Fix(BVar(0)), BVar(0)))
    testTermParsing("""letrec x = *x in *x""", Let(Fix(BVar(0, 1)), BVar(0, 1)))
    testTermParsing("""*letrec x = *x in *x""", Let(Fix(BVar(0, 1)), BVar(0, 1), 1))
  }

  test("parsing bindings") {
    val in =
      """app = \x -> \y ->
      |    case x of {
      |      Nil()  -> y;
      |      Cons(x, xs) -> Cons(x, (app xs y))
      |    };""".stripMargin
    val expected = Map(
      "app" ->
        Abs(
          Abs(
            Case(
              BVar(1),
              List(
                (Ptr("Nil", List()), BVar(0)),
                ((Ptr("Cons", List("x", "xs")), Ctr("Cons", List(BVar(1), App(App(GVar("app"), BVar(0)), BVar(2)))))),
              ),
            )
          )
        )
    )
    testBindingsParsing(in, expected)
  }

  test("parsing bindings with ticks") {
    val in =
      """app = \x -> \y ->
        |    **case *x of {
        |      Nil()  -> *y;
        |      Cons(x, xs) -> Cons(x, **(app xs y))
        |    };""".stripMargin
    val expected = Map(
      "app" ->
        Abs(
          Abs(
            Case(
              BVar(1, 1),
              List(
                (Ptr("Nil", List()), BVar(0, 1)),
                ((Ptr("Cons", List("x", "xs")), Ctr("Cons", List(BVar(1), App(App(GVar("app"), BVar(0)), BVar(2), 2))))),
              ),
              2,
            )
          )
        )
    )
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
