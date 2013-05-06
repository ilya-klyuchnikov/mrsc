package mrsc.pfp.test

import org.scalatest.FunSuite
import mrsc.core.test.DebugInfo
import mrsc.pfp._

class TicksNormalizationSuite extends FunSuite with DebugInfo {
  import NamedSyntax._

  def testNorm(t: Term, expected: Term) {
    val tNorm = TicksNorm.norm(t)
    info(s"\n${named(t)} => \n${named(tNorm)}")
    assert(expected === tNorm)
  }

  test("case expression normalization 1") {
    val t1: Term = "*case <1> of {Z() -> Z(); S(x) -> S(x)}"
    val t2: Term = "case <1> of {Z() -> *Z(); S(x) -> *S(x)}"
    val t1Ruled = TicksNorm.caseNorm(t1)
    assert(t2 === t1Ruled)

    val t1Norm = TicksNorm.norm(t1)
    assert(t2 === t1Norm)
  }

  test("case expression normalization 2") {
    val t1: Term = "*case **<1> of {Z() -> Z(); S(x) -> S(x)}"
    val t2: Term = "case <1> of {Z() -> ***Z(); S(x) -> ***S(x)}"
    val t1Ruled = TicksNorm.caseNorm(t1)
    assert(t2 === t1Ruled)

    val t1Norm = TicksNorm.norm(t1)
    assert(t2 === t1Norm)
  }

  test("letrec 1") {
    val t1: Term = """letrec f = \x -> *(g (f x)) in (f <1>)"""
    val t2: Term = """letrec f = \x -> (g *(f x)) in *(f <1>)"""
    val t1Ruled = TicksNorm.letRecNorm1(t1)
    info(s"${named(t1)} => ${named(t1Ruled)}")
    assert(t2 === t1Ruled)

    val t1Norm = TicksNorm.norm(t1)
    assert(t2 === t1Norm)
  }

  test("letrec 2") {
    val t1: Term =
      """(letrec f1 = (\x3 -> **(letrec f2 = (\x4 -> (\x5 -> case x4 of {S(x10) -> *((f2 x10) x5); Z() -> case x5 of {S(x10) -> (f1 x10); Z() -> True()}})) in ((f2 x3) x3))) in (f1 <1>))"""
    val t2: Term =
      """(letrec f1 = (\x3 -> (letrec f2 = (\x4 -> (\x5 -> case x4 of {S(x10) -> *((f2 x10) x5); Z() -> case x5 of {S(x10) -> **(f1 x10); Z() -> True()}})) in ((f2 x3) x3))) in **(f1 <1>))"""
    val t1Ruled = TicksNorm.letRecNorm1(t1)
    info(s"\n${named(t1)} => \n${named(t1Ruled)}")
    assert(t2 === t1Ruled)

    val t1Norm = TicksNorm.norm(t1)
    assert(t2 === t1Norm)
  }

  test("normalization") {
    testNorm(
      """letrec f2 = (\x4 -> (\x5 -> case x4 of {S(x10) -> *((f2 x10) x5); Z() -> **case x5 of {S(x10) -> *(f1 x10); Z() -> True()}})) in f2 x3 x3""",
      """letrec f2 = (\x4 -> (\x5 -> case x4 of {S(x10) -> *((f2 x10) x5); Z() -> case x5 of {S(x10) -> ***(f1 x10); Z() -> **True()}})) in f2 x3 x3"""
    )

    testNorm(
      """case <1> of {S(x0) -> *(letrec f0 = (\x1 -> (\x2 -> case x1 of {S(x10) -> *((f0 x10) x2); Z() -> (letrec f1 = (\x3 -> **(letrec f2 = (\x4 -> (\x5 -> case x4 of {S(x10) -> *((f2 x10) x5); Z() -> case x5 of {S(x10) -> (f1 x10); Z() -> True()}})) in ((f2 x3) x3))) in (f1 x2))})) in ((f0 x0) x0)); Z() -> True()}""",
      """case <1> of {S(x0) -> (letrec f0 = (\x1 -> (\x2 -> case x1 of {S(x10) -> *((f0 x10) x2); Z() -> (letrec f1 = (\x3 -> (letrec f2 = (\x4 -> (\x5 -> case x4 of {S(x10) -> *((f2 x10) x5); Z() -> case x5 of {S(x10) -> **(f1 x10); Z() -> True()}})) in ((f2 x3) x3))) in **(f1 x2))})) in *((f0 x0) x0)); Z() -> True()}"""
    )

    testNorm(
      """case <1> of {S(x0) -> *(letrec f0 = (\x1 -> (\x2 -> case x1 of {S(x10) -> *((f0 x10) x2); Z() -> **(letrec f1 = (\x3 -> *(letrec f2 = (\x4 -> (\x5 -> case x4 of {S(x10) -> *((f2 x10) x5); Z() -> **case x5 of {S(x10) -> (f1 x10); Z() -> True()}})) in ((f2 x3) x3))) in (f1 x2))})) in ((f0 x0) x0)); Z() -> **True()}""",
      """case <1> of {S(x0) -> (letrec f0 = (\x1 -> (\x2 -> case x1 of {S(x10) -> *((f0 x10) x2); Z() -> (letrec f1 = (\x3 -> (letrec f2 = (\x4 -> (\x5 -> case x4 of {S(x10) -> *((f2 x10) x5); Z() -> case x5 of {S(x10) -> ***(f1 x10); Z() -> **True()}})) in ((f2 x3) x3))) in ***(f1 x2))})) in *((f0 x0) x0)); Z() -> **True()}"""
    )

  }


}
