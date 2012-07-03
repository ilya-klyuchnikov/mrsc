package mrsc.pfp.samples

import mrsc.core._
import mrsc.pfp._
import scala.annotation.tailrec

class ClassicSC(val gc: GContext) extends PFPRules
  with PFPSyntax
  with PFPSemantics
  with PositiveDriving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HEByCouplingWhistle
  with UpperMsgOrLowerMggOnBinaryWhistle

object REPL {

  var trace: Boolean = false

  def main(args: Array[String]) {
    loop()
  }

  @tailrec
  private def loop() {
    invite()
    Console.readLine() match {
      case ":q" =>
        return
      case ":c" =>
        clear()
      case ":t" =>
        listTasks()
      case ":trace+" =>
        trace = true
      case ":trace-" =>
        trace = false
      case ":all" =>
        runAll()
      case name if tasks.tasks.get(name).isDefined =>
        val task = tasks(name)
        runTask(task)
      case cmd =>
        Console.println("unknown cmd")
    }
    loop()
  }

  private def invite() {
    if (trace)
      Console.print("pfpc+> ")
    else
      Console.print("pfpc > ")
  }

  private def runTask(task: Task) {
    Console.println(task.name)
    Console.println(task.goal)

    val rules = new ClassicSC(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, trace).toList
    val sGraph = graphs.head
    val tGraph = Transformations.transpose(sGraph)
    
    Console.println(tGraph)
    

    if (trace) {
      history = Tracing.history(sGraph).reverse
      current = -1
      steps()
    }
    
    Console.print(Console.BOLD)
    val result = Residuator(tGraph).result
    Console.println(result.toString())
    Console.print(Console.RESET)
  }

  var current = 0
  var history: List[SGraph[_, _]] = _

  @tailrec
  private def steps() {
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
    steps()
  }

  private def clear() {
    Console.println("\033[2J")
    Console.println("\033[0;0H")
    Console.flush()
  }

  private def listTasks() {
    val ts = tasks.tasks.values.toList.sortBy(_.name)
    ts map { t => Console.println(t.name) }
  }

  private def runAll() {
    val ts = tasks.tasks.values.toList.sortBy(_.name)
    ts map { runTask }
  }
}
