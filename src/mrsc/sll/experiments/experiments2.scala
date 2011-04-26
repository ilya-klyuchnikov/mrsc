package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

object experiments2 extends App {
  
  val t1 = SLLTasks.namedTasks("gApp(x, y)")
  //countPrograms(t1, new PSLLMultiMachine(program = t1.program))
  
  val t2 = SLLTasks.namedTasks("gApp(gApp(x, x), x)")
  //countPrograms(t2, new PSLLMultiMachine(program = t2.program, whistle = HEWithRedexWhistle))
  
  val t3 = SLLTasks.namedTasks("gOr(gEven(x), gOdd(x))")
  countPrograms(t3, new PSLLMultiMachine(program = t3.program, whistle = HEWithRedexWhistle))
  
  def countPrograms(task: SLLTask, machine: PSLLMultiMachine) = {
    println(task.target)
    //println(machine.name)
    try {
      val consumer = new CountProgramConsumer2()
      val builder = new MultiCoGraphBuilder(machine, consumer)
      builder.buildCoGraph(task.target, TransientStep)
      consumer.showResults()
      println()
    } catch {
      case e: ModelingError =>
        Console.println("ERR:" + e.message)
        println()
    }
  }
}