package mrsc.pfp.test

import mrsc.core._
import mrsc.pfp._
import mrsc.pfp.NamelessShows._
import org.scalatest.FunSuite
import mrsc.core.test.DebugInfo
import scalaz.Show

/**
 * Smoke tests for correctness
 */
trait MRSCNatHelper extends FunSuite with DebugInfo {

  val bindings = io.bingingsFromFile("pfp/defs/nat.pfp")
  val prettyPrinter = new PFPGraphPrettyPrinter {
    implicit def termShow[T <: MetaTerm]: Show[T] = NamelessShows.TermShow
  }

  def allSubs(fvs: List[FVar], seed: Int): List[Subst] =
    cartesianProduct(fvs.map(_ => values(seed))) map {vs => Map(fvs zip vs:_*)}

  def values(seed: Int): List[Term] =
    (0 to seed).toList.map(nat)

  def nat(i: Int): Term =
    if (i == 0) Ctr("Z", Nil) else Ctr("S", nat(i - 1) :: Nil)

  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for(xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
  }

  def checkAllSc(in: String, sc: PFPSC, seed: Int) {
    val goal = PFPParsers().inputTerm(in)
    info(s"${s(goal)}")
    val rules = sc(bindings)
    val graphs = GraphGenerator(rules, goal)
    var checked = 0
    for { sGraph <- graphs } {
      val tGraph = Transformations.transpose(sGraph)
      val simpleResidual = Residuator(tGraph).result
      val tickedResidual = Residuator(tGraph, true).result
      checked += 1
      //info(s"checking output #$checked")
      //info(prettyPrinter.toString(tGraph))
      info(NamedSyntax.named(tickedResidual))
      checkCorrectness(goal, simpleResidual, seed)
      checkTicks(goal, tickedResidual, seed)
    }
    info(s"checked $checked outputs")
  }

  def checkImprovementSc(in1: String, in2: String, sc: PFPSC) {
    val goal1 = PFPParsers().inputTerm(in1)
    val goal2 = PFPParsers().inputTerm(in2)
    info(s"${s(goal1)}")
    info(s"${s(goal2)}")
    val rules = sc(bindings)
    val sGraph1 = GraphGenerator(rules, goal1).toList.head
    val tGraph1 = Transformations.transpose(sGraph1)
    val tickedResidual1 = Residuator(tGraph1, true).result

    val sGraph2 = GraphGenerator(rules, goal2).toList.head
    val tGraph2 = Transformations.transpose(sGraph2)
    val tickedResidual2 = Residuator(tGraph2, true).result


    info(s"${s(tickedResidual1)}")
    info(s"${s(tickedResidual2)}")

    val residual1 = Ticks.reset(tickedResidual1)
    val residual2 = Ticks.reset(tickedResidual2)

    val tickedResidual1N = TicksNorm.norm(tickedResidual1)
    val tickedResidual2N = TicksNorm.norm(tickedResidual2)

    info(s"${NamedSyntax.named(tickedResidual1)}")
    info(s"${NamedSyntax.named(tickedResidual2)}")

    assert(residual1 === residual2)
    info("normalized")
    //info(s"${NamedSyntax.named(tickedResidual1N)}")
    //info(s"${NamedSyntax.named(tickedResidual2N)}")
    assert(Ticks.isImprovement(tickedResidual1N, tickedResidual2N))
  }

  def checkAllImprovementSc(in1: String, in2: String, sc: PFPSC, seed: Int) {
    val goal1 = PFPParsers().inputTerm(in1)
    val goal2 = PFPParsers().inputTerm(in2)
    info(s"${s(goal1)}")
    info(s"${s(goal2)}")
    val rules = sc(bindings)
    val sGraph1 = GraphGenerator(rules, goal1).toList.head
    val tGraph1 = Transformations.transpose(sGraph1)
    val tickedResidual1 = Residuator(tGraph1, true).result

    val sGraph2 = GraphGenerator(rules, goal2).toList.head
    val tGraph2 = Transformations.transpose(sGraph2)
    val tickedResidual2 = Residuator(tGraph2, true).result

    info(s"${s(tickedResidual1)}")
    info(s"${s(tickedResidual2)}")

    info(s"${NamedSyntax.named(tickedResidual1)}")
    info(s"${NamedSyntax.named(tickedResidual2)}")

    checkImprovement(tickedResidual1, tickedResidual2, seed)
  }

  def checkAllDepth(in: String, depth: Int, seed: Int) {
    val goal = PFPParsers().inputTerm(in)
    info(s"${s(goal)}, depth=${depth}, seed=${seed}")
    val rules = new DepthBoundMRSC(bindings, depth)
    val graphs = GraphGenerator(rules, goal)
    var checked = 0
    for { sGraph <- graphs } {
      val tGraph = Transformations.transpose(sGraph)
      val simpleResidual = Residuator(tGraph).result
      val tickedResidual = Residuator(tGraph, true).result
      checked += 1
      //info(s"checking output #$checked")
      //info(prettyPrinter.toString(tGraph))
      //info(NamedSyntax.named(tickedResidual))
      checkCorrectness(goal, simpleResidual, seed)
      checkTicks(goal, tickedResidual, seed)
    }
    info(s"checked $checked outputs")
  }

  def checkDebug(in: String, steps: List[Int]) {
    val goal = PFPParsers().inputTerm(in)
    info(s(goal))
    val rules = new DebugMRSC(bindings, steps)
    val sGraph = GraphGenerator(rules, goal).toList.head


    val tGraph = Transformations.transpose(sGraph)
    val simpleResidual = Residuator(tGraph).result
    val tickedResidual = Residuator(tGraph, true).result

    //info(prettyPrinter.toString(tGraph))
    //info(NamedSyntax.named(tickedResidual))
    checkCorrectness(goal, simpleResidual, 2)
    checkTicks(goal, tickedResidual, 2)
  }

  def checkCorrectness(goal: Term, residual: Term, seed: Int) {
    import NamelessSyntax._

    val fvs = freeVars(goal)
    val subs = allSubs(fvs, seed)

    for {sub <- subs} {
      val val1 = CBNEval.eval(applySubst(goal, sub), bindings)
      val val2 = CBNEval.eval(applySubst(residual, sub), Map())
      assert(NamedSyntax.named(val1) === NamedSyntax.named(val2))
    }
  }

  def checkImprovement(t1: Term, t2: Term, seed: Int) {
    import NamelessSyntax._

    val fvs = freeVars(t1)
    val subs = allSubs(fvs, seed)

    for {sub <- subs} {
      val e1 = applySubst(t1, sub)
      val e2 = applySubst(t2, sub)

      //info(s(e1))
      //info(s(e2))
      //info(NamedSyntax.named(e2))

      val (ticks1, evaled1) = CBNEvalWithTicks.eval(e1, bindings)
      val (ticks2, evaled2) = CBNEvalWithTicksResidual.eval(e2, Map())
      assert(evaled1 === evaled2)
      //info(s"$ticks1, $ticks2")
      assert(ticks1 <= ticks2)
    }
  }

  def checkTicks(goal: Term, residual: Term, seed: Int) {
    import NamelessSyntax._

    val fvs = freeVars(goal)
    val subs = allSubs(fvs, seed)

    for {sub <- subs} {
      val e1 = applySubst(goal, sub)
      val e2 = applySubst(residual, sub)

      //info(s(e1))
      //info(s(e2))
      //info(NamedSyntax.named(e2))

      val (ticks1, evaled1) = CBNEvalWithTicks.eval(e1, bindings)
      val (ticks2, evaled2) = CBNEvalWithTicksResidual.eval(e2, Map())
      assert(evaled1 === evaled2)
      assert(ticks1 === ticks2)
    }
  }

}

class MRSCNatSuite extends MRSCNatHelper {


  test("tick normalization") {
    val t1 = PFPParsers().inputTerm(
      """(letrec f1 = (\x3 -> **(letrec f2 = (\x4 -> (\x5 -> case x4 of {S(x10) -> *((f2 x10) x5); Z() -> case x5 of {S(x10) -> (f1 x10); Z() -> True()}})) in ((f2 x3) x3))) in (f1 <1>))"""
    )
    val t2 = PFPParsers().inputTerm(
      """(letrec f1 = (\x3 -> (letrec f2 = (\x4 -> (\x5 -> case x4 of {S(x10) -> *((f2 x10) x5); Z() -> case x5 of {S(x10) -> **(f1 x10); Z() -> True()}})) in ((f2 x3) x3))) in **(f1 <1>))"""
    )
    info(NamedSyntax.named(t1))
    info(NamedSyntax.named(t2))
    checkImprovement(t1, t2, 10)
    checkImprovement(t2, t1, 10)
  }

  // TODO: after tick normalization we should be able to prove that these are improvement lemmas
  test("improvement lemmas") {
    /*
    checkAllImprovementSc(
      "case case <1> of {S(x) -> (fin1 x); Z() -> True()} of {False() -> False(); True() -> case <1> of {S(x) -> (fin2 x); Z() -> True()}}",
      "case case <1> of {S(x) -> (fin1 x); Z() -> True()} of {False() -> False(); True() -> case S(<1>) of {S(x) -> (fin2 x); Z() -> True()}}",
      SC2,
      5
    )
    */

    checkImprovementSc(
      "case case <1> of {S(x) -> (fin1 x); Z() -> True()} of {False() -> False(); True() -> case <1> of {S(x) -> (fin2 x); Z() -> True()}}",
      "case case <1> of {S(x) -> (fin1 x); Z() -> True()} of {False() -> False(); True() -> case S(<1>) of {S(x) -> (fin2 x); Z() -> True()}}",
      SC2
    )

    /*
    checkAllImprovementSc(
      "case S(<1>) of {S(x) -> (fin2 x); Z() -> True()}",
      "case S(S(<1>)) of {S(x) -> (fin2 x); Z() -> True()}",
      SC2,
      5
    )*/
  }

  test("fin1 x") {
    checkAllDepth("fin1 <1>", 7, 4)
    checkDebug("fin1 <1>", List(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  }

  test("fin2 x") {
    checkAllDepth("fin2 <1>", 7, 4)
  }

  test("even x") {
    checkAllDepth("even <1>", 8, 4)
  }

  test("even (plus x y)") {
    checkAllDepth("even (plus <1> <2>)", 8, 4)
  }

  test("even (plus x x)") {
    checkAllDepth("even (plus <1> <1>)", 9, 4)
  }

  test("eq (plus x x) x") {
    checkAllDepth("eq (plus <1> <1>) <1>", 3, 4)
  }


}
