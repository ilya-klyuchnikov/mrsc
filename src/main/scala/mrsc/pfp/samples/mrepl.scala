package mrsc.pfp.samples

import mrsc.core._
import mrsc.pfp._
import scala.annotation.tailrec

// REPL for interactive experiments with
// single-result supercompilers
object MREPL {

  def msc(sc: SC, t: String) {
    if (!tasks.tasks.get(t).isDefined) {
      Console.println("no such task")
      return
    }
    
    val task = tasks(t)
    Console.println(task.name)
    Console.println(task.goal)

    val rules = sc(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, false)

    var count = 0
    var uniques: Set[Term] = Set()
    for { sGraph <- graphs } {
      val tGraph = Transformations.transpose(sGraph)
      val result = Residuator(tGraph).result

      Console.println(tGraph.toString())
      Console.println(result.toString())
      Console.println(NamedSyntax.named(result))
      count += 1
      Console.println(count)
      uniques = uniques + result
      Console.println("size = " + uniques.size)
    }

  }

  def ls() {
    val ts = tasks.tasks.values.toList.sortBy(_.name)
    ts map { t => Console.println(t.name + ": " + t.goal) }
  }
}
