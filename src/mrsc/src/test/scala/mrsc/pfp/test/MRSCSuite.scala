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

  def checkAll(in: String, depth: Int, seed: Int) {
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

  test("fin1 x") {
    checkAll("fin1 <1>", 7, 4)
    checkDebug("fin1 <1>", List(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  }

  test("fin2 x") {
    checkAll("fin2 <1>", 7, 4)
  }

  test("even x") {
    checkAll("even <1>", 8, 4)
  }

  test("even (plus x y)") {
    checkAll("even (plus <1> <2>)", 8, 4)
  }

  test("even (plus x x)") {
    checkAll("even (plus <1> <1>)", 9, 4)
  }


}
