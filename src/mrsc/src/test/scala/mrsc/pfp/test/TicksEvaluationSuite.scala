package mrsc.pfp.test

import org.scalatest.FunSuite

import mrsc.pfp._
import NamelessShows._
import mrsc.core._
import mrsc.test.DebugInfo

class TicksEvaluationSuite extends FunSuite with DebugInfo {
  case class SC2(val gc: GContext) extends PFPRules
  with PFPSemantics
  with PositiveDriving
  with AllFoldingCandidates
  with Folding
  with ControlEmbeddingCandidates
  with HE3ByCouplingWhistle
  with LowerMsgOrUpperMsgOnBinaryWhistle

  val bindings = io.bingingsFromFile("pfp/defs/list.pfp")

  def checkTicks(goal1: Term, goal2: Term, sub: Subst) {
    val e1 = NamelessSyntax.applySubst(goal1, sub)
    val e2 = NamelessSyntax.applySubst(goal2, sub)

    val (ticks1, evaled1) = CBNEvalWithTicks.eval(e1, bindings)
    val (ticks2, evaled2) = CBNEvalWithTicksResidual.eval(e2, Map())
    debug(s"${s(e1)} ==> ${ticks1} ticks")
    debug(s"${s(e2)} ==> ${ticks2} ticks")
    assert(evaled1 === evaled2)
    assert(ticks1 === ticks2)
  }


  test("simple evaluation #1") {
    val goal = PFPParsers().inputTerm("app Nil() Nil()")
    val expected = PFPParsers().inputTerm("Nil()")
    info(s(goal))
    info(s(expected))
    val (ticks, evaled) = CBNEvalWithTicks.eval(goal, bindings)
    info(s(evaled))
    info(ticks.toString)
    assert(evaled === expected)
    assert(1 === ticks)
  }

  test("simple evaluation #2") {
    val goal = PFPParsers().inputTerm("app (app Cons(A(), Nil()) Cons(A(), Nil()) ) Cons(A(), Nil())")
    val expected = PFPParsers().inputTerm("Cons(A(), Cons(A(), Cons(A(), Nil())))")
    info(s(goal))
    info(s(expected))
    val (ticks, evaled) = CBNEvalWithTicks.eval(goal, bindings)
    info(s(evaled))
    info(ticks.toString)
    assert(evaled === expected)
    assert(5 === ticks)
  }

  // less ticks: just 4
  test("simple evaluation #3") {
    val goal = PFPParsers().inputTerm("app Cons(A(), Nil()) (app Cons(A(), Nil()) Cons(A(), Nil()) )")
    val expected = PFPParsers().inputTerm("Cons(A(), Cons(A(), Cons(A(), Nil())))")
    info(s(goal))
    info(s(expected))
    val (ticks, evaled) = CBNEvalWithTicks.eval(goal, bindings)
    info(s(evaled))
    info(ticks.toString)
    assert(evaled === expected)
    assert(4 === ticks)
  }

  test("evaluation after deforestation") {
    val goal = PFPParsers().inputTerm("app (app <1> <2>) <3>")
    val rules = new Deforester(bindings)
    val g = GraphGenerator(rules, goal).toList.head
    val tg = Transformations.transpose(g)

    val result: Term = Residuator(tg, true).result

    val t1: Term = "Cons(A(), Nil())"
    val t2: Term = "Cons(A(), Cons(A(), Nil()))"
    val sub1: Subst = Map(FVar(1) -> t1, FVar(2) -> t1, FVar(3) -> t1)
    val sub2: Subst = Map(FVar(1) -> t2, FVar(2) -> t2, FVar(3) -> t2)

    checkTicks(goal, result, sub1)
    checkTicks(goal, result, sub2)

  }

  test("evaluation after hard supercompilation") {
    val goal = PFPParsers().inputTerm("fin2 <1>")
    val rules = new SC2(bindings)
    val g = GraphGenerator(rules, goal).toList.head
    val tg = Transformations.transpose(g)

    val result: Term = Residuator(tg, true).result

    val t0: Term = "Z()"
    val t1: Term = "S(Z())"
    val t2: Term = "S(S(Z()))"
    val t3: Term = "S(S(S(Z())))"

    val sub0: Subst = Map(FVar(1) -> t0)
    val sub1: Subst = Map(FVar(1) -> t1)
    val sub2: Subst = Map(FVar(1) -> t2)
    val sub3: Subst = Map(FVar(1) -> t3)

    checkTicks(goal, result, sub0)
    checkTicks(goal, result, sub1)
    checkTicks(goal, result, sub2)
    checkTicks(goal, result, sub3)

  }

  test("evaluation after hard supercompilation #1") {
    val goal = PFPParsers().inputTerm("case case <1> of {S(x) -> (fin1 x); Z() -> True()} of {False() -> False(); True() -> case <1> of {S(x) -> (fin2 x); Z() -> True()}}")
    val rules = new SC2(bindings)
    val g = GraphGenerator(rules, goal).toList.head
    val tg = Transformations.transpose(g)

    val result: Term = Residuator(tg, true).result

    val t0: Term = "Z()"
    val t1: Term = "S(Z())"
    val t2: Term = "S(S(Z()))"
    val t3: Term = "S(S(S(Z())))"

    val sub0: Subst = Map(FVar(1) -> t0)
    val sub1: Subst = Map(FVar(1) -> t1)
    val sub2: Subst = Map(FVar(1) -> t2)
    val sub3: Subst = Map(FVar(1) -> t3)

    checkTicks(goal, result, sub0)
    checkTicks(goal, result, sub1)
    checkTicks(goal, result, sub2)
    checkTicks(goal, result, sub3)

  }

  test("evaluation after hard supercompilation #2") {
    val goal = PFPParsers().inputTerm("case case <1> of {S(x) -> (fin1 x); Z() -> True()} of {False() -> False(); True() -> case S(<1>) of {S(x) -> (fin2 x); Z() -> True()}}")
    val rules = new SC2(bindings)
    val g = GraphGenerator(rules, goal).toList.head
    val tg = Transformations.transpose(g)

    val result: Term = Residuator(tg, true).result

    val t0: Term = "Z()"
    val t1: Term = "S(Z())"
    val t2: Term = "S(S(Z()))"
    val t3: Term = "S(S(S(Z())))"

    val sub0: Subst = Map(FVar(1) -> t0)
    val sub1: Subst = Map(FVar(1) -> t1)
    val sub2: Subst = Map(FVar(1) -> t2)
    val sub3: Subst = Map(FVar(1) -> t3)

    checkTicks(goal, result, sub0)
    checkTicks(goal, result, sub1)
    checkTicks(goal, result, sub2)
    checkTicks(goal, result, sub3)

  }
}