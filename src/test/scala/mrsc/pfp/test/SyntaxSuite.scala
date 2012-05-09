package mrsc.pfp.test

import org.scalatest.FunSuite

import mrsc.pfp._

class SyntaxSuite extends FunSuite {
  test("checking for extraction") {
    val t1: Term = """\x -> x"""
    val extractable = Syntax.isFreeSubTerm(t1, 0)
    info(extractable.toString())
  }

  test("finding substitutions") {
    testSubst(
      """ \x -> x """,
      """ \y -> y """,
      Some(Map()))

    testSubst(
      """ \x -> <1> """,
      """ \y -> <2> """,
      Some(Map[Term, Term](t("<1>") -> t("<2>"))))

    testSubst(
      """ \x -> <1> <1> """,
      """ \y -> <2> <3> """,
      None)

    testSubst(
      """ \x -> <1>   <2> """,
      """ \y -> Nil[] <3> """,
      Some(Map[Term, Term](t("<1>") -> t("Nil[]"), t("<2>") -> t("<3>"))))

    testSubst(
      """ \x -> <2> """,
      """ \y ->  y """,
      None)

    testSubst(
      """ <1> """,
      """ \y ->  y """,
      Some(Map(t("""<1>""") -> t("""\y -> y"""))))

    testSubst(
      """ \x -> <1> """,
      """ \y ->  \z -> z """,
      Some(Map(t("""<1>""") -> t("""\z -> z"""))))

    testSubst(
      """ \x -> <1> """,
      """ \y ->  \z -> y """,
      None)

    testSubst(
      """ <1> """,
      """ <2>.tail """,
      Some(Map(t("<1>") -> t("<2>.tail"))))

    testSubst(
      """ <1>.tail """,
      """ <2> """,
      Some(Map(t("<1>.tail") -> t("<2>"))))

    testSubst(
      """ app <11> <12> """,
      """ app <1> <2> """,
      Some(Map(t("<11>") -> t("<1>"), t("<12>") -> t("<2>"))))

    testSubst(
      """ app <11> <12> """,
      """ app <1>.tail <12> """,
      Some(Map(t("<11>") -> t("<1>.tail"))))
  }

  test("renaming #1") {
    val t1 = """ app <11> <12> """
    val t2 = """ app <1>.tail <12> """
    val ren = Syntax.renaming(t1, t2)
    assert(ren === true)
  }

  private def testSubst(in1: String, in2: String, sub: Option[Subst]): Unit = {
    val t1: Term = in1
    val t2: Term = in2
    val sub1 = Syntax.findSubst(t1, t2)
    info(in1 + " ^ " + in2 + " = " + sub1)
    assert(sub1 === sub)
  }

}