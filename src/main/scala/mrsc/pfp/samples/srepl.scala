package mrsc.pfp.samples

import mrsc.core._
import mrsc.pfp._
import scala.annotation.tailrec

// REPL for interactive experiments with
// single-result supercompilers
object SREPL {

  def main(args: Array[String]) {
    loop()
  }

  @tailrec
  private def loop() {
    Console.print("pfpc > ")
    Console.readLine() match {
      // quit
      case ":q" =>
        return
      // clear console
      case ":c" =>
        clear()
      // list all tasks
      case ":ls" =>
        listTasks()
      case ":all" =>
        runAll()
      // tracing the last supercompilation task
      case ":trace" if history != null =>
        clear()
        trace()
      // running task by name
      case name if tasks.tasks.get(name).isDefined =>
        val task = tasks(name)
        runTask(task)
      case _ =>
        Console.println("unknown cmd")
    }
    loop()
  }

  private def runTask(task: Task) {
    Console.println(task.name)
    Console.println(task.goal)

    val rules = ClassicSC(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, true).toList
    val sGraph = graphs.head
    val tGraph = Transformations.transpose(sGraph)

    history = Tracing.history(sGraph).reverse
    current = -1

    Console.println(tGraph)
    Console.print(Console.BOLD)
    val result = Residuator(tGraph).result
    Console.println(result.toString())
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

  private def listTasks() {
    val ts = tasks.tasks.values.toList.sortBy(_.name)
    ts map { t => Console.println(t.name + ": " + t.goal) }
  }

  private def runAll() {
    val ts = tasks.tasks.values.toList.sortBy(_.name)
    ts map { runTask }
  }
}