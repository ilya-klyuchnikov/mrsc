package mrsc.sll.experiments

import mrsc.sll._
import SLLSyntax._

object Checker {

  private def sllNumber(n: Int): Expr = n match {
    case 0 => Ctr("Z", List())
    case _ => Ctr("S", List(sllNumber(n - 1)))
  }

  val ns = (0 to 4 map sllNumber).toList

  // very naive
  private def lists(size: Int): List[Expr] = size match {
    case 0 =>
      List(Ctr("Nil", List()))
    case _ =>
      val prevs = lists(size - 1)
      for (n <- ns; l <- prevs) yield Ctr("Cons", List(n, l))
  }

  val ls = (0 to 4 map lists).toList.flatten.toList

  def main(args: Array[String]) {
    ns.map(println)
    println()
    ls.map(println)
    
    val vs = List(Var("m"), Var("n"))
    val ss = subs(vs)
    ss.foreach(println)
  }

  private def values(v: Var): List[Expr] = v match {
    case Var("m") => ns
    case Var("n") => ns
    case Var("p") => ns
    case Var("q") => ns
    case _ => ls
  }

  private def subs(vs: List[Var]): List[Map[Var, Expr]] = vs match {
    case Nil => 
      List(Map())
    case v :: vs1 => 
      val ss1 = values(v)
      val ss2 = subs(vs1)
      for (value <- ss1; sub <- ss2) yield (sub + (v -> value))
  }
  
  def check(t1: SLLTask, t2: SLLTask) {
    val vs = vars(t1.target)
    val ss = subs(vs)
    for (sub <- ss) {
      val e1 = subst(t1.target, sub map {case (k, v) => (k.name, v)})
      val e2 = subst(t2.target, sub map {case (k, v) => (k.name, v)})
      
      val task1 = SLLTask(e1, t1.program)
      val task2 = SLLTask(e2, t2.program)
      
      val res1 = SLLInterpreter.eval(task1)
      val res2 = SLLInterpreter.eval(task2)
      
      //println("???")
      //println(task1)
      //println(task2)
      //println("***")
      
      require(res1 == res2, "goal: " + task1.target + " expected: " + res1 + ", but was: " + res2)
    }
  }
}