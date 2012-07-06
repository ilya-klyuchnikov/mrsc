package mrsc.pfp.samples

import mrsc.core._
import mrsc.pfp._
import scala.annotation.tailrec

// REPL for interactive experiments with
// single-result supercompilers
object SREPL {

  def sc(sc: SC, t: String) {
    if (!tasks.tasks.get(t).isDefined) {
      Console.println("no such task")
      return
    }
    val task = tasks(t)
    runTask(sc, task)
  }

  private def runTask(sc: SC, task: Task) {
    Console.println(task.name)
    Console.println(task.goal)

    val rules = sc(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, true).toList
    val sGraph = graphs.head
    val tGraph = Transformations.transpose(sGraph)

    history = Tracing.history(sGraph).reverse
    current = -1

    Console.println(tGraph)
    Console.print(Console.BOLD)
    val result = Residuator(tGraph).result
    Console.println(result)
    Console.println(NamedSyntax.named(result))
    Console.print(Console.RESET)
  }

  var current = 0
  var history: List[SGraph[_, _]] = _

  @tailrec
  private def trace() {
    Console.readLine() match {
      case "" if current < history.size - 1 =>
        clear()
        current += 1
        val sGraph = history(current)
        val tGraph = Transformations.transpose(sGraph)
        Console.println(tGraph.toString)
      case ":p" if current > 0 =>
        clear()
        current -= 1
        val sGraph = history(current)
        val tGraph = Transformations.transpose(sGraph)
        Console.println(tGraph.toString)
      case _ =>
        return
    }
    trace()
  }

  private def clear() {
    Console.println("\033[2J")
    Console.println("\033[0;0H")
    Console.flush()
  }

  def ls() {
    val ts = tasks.tasks.values.toList.sortBy(_.name)
    ts map { t => Console.println(t.name + ": " + t.goal) }
  }

  def scAll(sc: SC) {
    val ts = tasks.tasks.values.toList.sortBy(_.name)
    ts map { runTask(sc, _) }
  }
}
