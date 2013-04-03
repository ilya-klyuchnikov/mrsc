package misc

import mrsc.pfp._
import mrsc.core.{Transformations, GraphGenerator}

// checks the correctness of simple programs
// assumes that all free variables in a program are natural numbers
// and that a program terminates
object SimpleChecker {

  def run(f: String, sc: PFPSC) {
    import scala.io.Source
    val text = Source.fromFile(f).mkString
    val task = PFPParsers().inputTask(text)
    Console.println(text)
    Console.println(task.goal)

    val rules = sc(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, false)

    var checked = 0
    for { sGraph <- graphs } {
      val tGraph = Transformations.transpose(sGraph)
      val result = Residuator(tGraph).result
      checked += 1
      check(task, Task(result, Map()))
    }

    Console.println("OK, checked %d results".format(checked))
  }

  def check(original: Task, transformed: Task, seed: Int = 2) {
    import NamelessSyntax._

    val fvs = freeVars(original.goal)
    val subs = allSubs(fvs, seed)

    for {sub <- subs} {
      val val1 = CBNEval.eval(applySubst(original.goal, sub), original.bindings)
      val val2 = CBNEval.eval(applySubst(transformed.goal, sub), transformed.bindings)
      assert(val1 == val2)
      Console.println(val1)
    }
  }

  // all possible subs
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
}
