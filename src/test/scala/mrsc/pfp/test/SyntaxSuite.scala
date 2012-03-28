package mrsc.pfp.test

import org.scalatest.FunSuite

import mrsc.pfp._

class SyntaxSuite extends FunSuite {
  test("checking for extraction") {
    val t1 = PFPParsers.inputTerm("""\x -> x""")
    val extractable = Syntax.isFreeSubTerm(t1, 0)
    info(extractable.toString())
  }

  test("finding substitutions #1") {
    val t11 = PFPParsers.inputTerm("""\x -> x""")
    val t12 = PFPParsers.inputTerm("""\y -> y""")
    val sub1 = Syntax.findSubst(t11, t12)
    info(sub1.toString())
  }

  test("finding substitutions #2") {
    val t11 = PFPParsers.inputTerm("""\x -> <a>""")
    val t12 = PFPParsers.inputTerm("""\y -> <b>""")
    val sub1 = Syntax.findSubst(t11, t12)
    info(sub1.toString())
  }

  test("finding substitutions #3") {
    val t11 = PFPParsers.inputTerm("""\x -> <a> <a>""")
    val t12 = PFPParsers.inputTerm("""\y -> <b> <c>""")
    val sub1 = Syntax.findSubst(t11, t12)
    info(sub1.toString())
  }

  test("finding substitutions #4") {
    val t11 = PFPParsers.inputTerm("""\x -> <a> <b>""")
    val t12 = PFPParsers.inputTerm("""\y -> Nil[] <c>""")
    val sub1 = Syntax.findSubst(t11, t12)
    info(sub1.toString())
  }
}