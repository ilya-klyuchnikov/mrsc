package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

object experiments2 extends App {

  val t1 = SLLTasks.namedTasks("gApp(x, y)")
  //countPrograms(t1, new PSLLMultiMachine(program = t1.program))

  val t2 = SLLTasks.namedTasks("gApp(gApp(x, x), x)")
  //countPrograms(t2, new PSLLMultiMachine(program = t2.program, whistle = HEWithRedexWhistle))

  val t3 = SLLTasks.namedTasks("gOr(gEven(x), gOdd(x))")
  //println(t3)
  //countPrograms(t3, new PSLLMultiMachine(program = t3.program))

  val t4 = SLLTasks.namedTasks("gRev(x)")

  val t5 = SLLTasks.namedTasks("gApp(gApp(gRev(xs), Cons(y, Nil())), ys)")
  /*
  countPrograms(t3, 
      new PSLLMultiMachine(program = t3.program, whistle = HEWithRedexWhistle, speculate = true, rebuilduingStrategy = RebuilduingStrategy.Never))
*/
  /*
  countPrograms(t3, new PSLLMultiMachine(program = t3.program, whistle = HEWithRedexWhistle, speculate = false))
  countPrograms(t3, new PSLLMultiMachine(program = t3.program,
    whistle = HEWithRedexWhistle,
    speculate = false,
    rebuilduingStrategy = RebuilduingStrategy.DangerousByWhistle))

  countPrograms(t4, new PSLLMultiMachine(program = t4.program, whistle = HEByCouplingWhistle, speculate = false))
  countPrograms(t4, new PSLLMultiMachine(program = t4.program,
    whistle = HEByCouplingWhistle,
    speculate = false,
    rebuilduingStrategy = RebuilduingStrategy.DangerousByWhistle))

  countPrograms(t5, new PSLLMultiMachine(program = t4.program, whistle = HEByCouplingWhistle, speculate = false))
  countPrograms(t5, new PSLLMultiMachine(program = t5.program,
    whistle = HEByCouplingWithRedexWhistle,
    speculate = false,
    rebuilduingStrategy = RebuilduingStrategy.DangerousByWhistle))
*/

  countPrograms(t3, new PSLLMultiMachine(program = t3.program,
    whistle = HEByCouplingWithRedexWhistle,
    speculate = false,
    rebuilduingStrategy = RebuilduingStrategy.CurrentByWhistle))

  countPrograms(t3, new PSLLMultiMachine(program = t3.program,
    whistle = HEByCouplingWithRedexWhistle,
    speculate = false,
    rebuilduingStrategy = RebuilduingStrategy.DangerousByWhistle,
    rebuilduingTactics = RebuilderTactics.AllGens))

  countPrograms(t3, new PSLLMultiMachine(program = t3.program,
    whistle = HEByCouplingWithRedexWhistle,
    speculate = false,
    rebuilduingStrategy = RebuilduingStrategy.CurrentByWhistle,
    rebuilduingTactics = RebuilderTactics.AllGens))

  countPrograms(t4, new PSLLMultiMachine(program = t4.program,
    whistle = HEByCouplingWithRedexWhistle,
    speculate = false,
    rebuilduingStrategy = RebuilduingStrategy.CurrentByWhistle))

  countPrograms(t4, new PSLLMultiMachine(program = t4.program,
    whistle = HEByCouplingWithRedexWhistle,
    speculate = false,
    rebuilduingStrategy = RebuilduingStrategy.DangerousByWhistle,
    rebuilduingTactics = RebuilderTactics.AllGens))

  countPrograms(t4, new PSLLMultiMachine(program = t4.program,
    whistle = HEByCouplingWithRedexWhistle,
    speculate = false,
    rebuilduingStrategy = RebuilduingStrategy.CurrentByWhistle,
    rebuilduingTactics = RebuilderTactics.AllGens))

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