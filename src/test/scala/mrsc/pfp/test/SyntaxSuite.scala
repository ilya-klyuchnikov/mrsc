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
      """ \x -> <a> """,
      """ \y -> <b> """,
      Some(Map[Term, Term](t("<a>") -> t("<b>"))))

    testSubst(
      """ \x -> <a> <a> """,
      """ \y -> <b> <c> """,
      None)

    testSubst(
      """ \x -> <a>   <b> """,
      """ \y -> Nil[] <c> """,
      Some(Map[Term, Term](t("<a>") -> t("Nil[]"), t("<b>") -> t("<c>"))))

    testSubst(
      """ \x -> <b> """,
      """ \y ->  y """,
      None)

    testSubst(
      """ <x> """,
      """ \y ->  y """,
      Some(Map(t("""<x>""") -> t("""\y -> y"""))))

    testSubst(
      """ \x -> <a> """,
      """ \y ->  \z -> z """,
      Some(Map(t("""<a>""") -> t("""\z -> z"""))))

    testSubst(
      """ \x -> <a> """,
      """ \y ->  \z -> y """,
      None)

    testSubst(
      """ <a> """,
      """ <b>.tail """,
      Some(Map(t("<a>") -> t("<b>.tail"))))

    testSubst(
      """ <a>.tail """,
      """ <b> """,
      Some(Map(t("<a>.tail") -> t("<b>"))))

    testSubst(
      """ app <x> <y> """,
      """ app <a> <b> """,
      Some(Map(t("<x>") -> t("<a>"), t("<y>") -> t("<b>"))))

    testSubst(
      """ app <x> <y> """,
      """ app <a>.tail <y> """,
      Some(Map(t("<x>") -> t("<a>.tail"))))
  }

  test("renaming #1") {
    val t1 = """ app <x> <y> """
    val t2 = """ app <a>.tail <y> """
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