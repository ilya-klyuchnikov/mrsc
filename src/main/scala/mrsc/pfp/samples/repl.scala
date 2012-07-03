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
  with LowerMsgOrUpperMsgOnBinaryWhistle

object REPL {
  
  var trace: Boolean = false
  
  def main(args: Array[String]) {
    loop()
  }

  @tailrec
  private def loop() {
    Console.print("pfpc> ")
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

  private def runTask(task: Task) {
    Console.println(task.name)
    Console.println(task.goal)
    
    val rules = new ClassicSC(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, trace).toList
    val sGraph = graphs.head
    val tGraph = Transformations.transpose(sGraph)
    val result = Residuator(tGraph).result
    
    Console.println(tGraph)
    Console.print(Console.BOLD)
    Console.println(result.toString())
    Console.print(Console.RESET)
    
    if (trace) {
      val traces = Tracing.history(sGraph).reverse
      for (tr <- traces) {
        val tg = Transformations.transpose(tr)
        Console.println("+++")
        Console.println(tg.toString)
      }
    }
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
    ts map {runTask}
  }
}
