package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

object SLLExperiments extends App {
  import SLLTasks._
  
  //runBaseMultiSuperCompiler(t, new SLLMachine(t.program, ExpressionSize(10)))
  //exp1()
  exp2()

  // just calculating results with different whistles
  def exp1(): Unit = {
    for (t <- SLLTasks.tasks) {
      countGraphs(t, new SLLMachine(t.program, ExpressionSize(8)))
      countGraphs(t, new SLLMachine(t.program, HEWhistle))
      countGraphs(t, new SLLMachine(t.program, HEWithRedexWhistle))
      countGraphs(t, new SLLMachine(t.program, HEByCouplingWhistle))
      countGraphs(t, new SLLMachine(t.program, HEByCouplingWithRedexWhistle))
    }
  }
  
  def exp2(): Unit = {
    for (t <- SLLTasks.tasks) {
      countPrograms(t, new SLLMachine(t.program, ExpressionSize(8)))
      countPrograms(t, new SLLMachine(t.program, HEWhistle))
      countPrograms(t, new SLLMachine(t.program, HEWithRedexWhistle))
      countPrograms(t, new SLLMachine(t.program, HEByCouplingWhistle))
      countPrograms(t, new SLLMachine(t.program, HEByCouplingWithRedexWhistle))
    }
  }

  def countGraphs(task: SLLTask, machine: SLLMachine) = {
    println(task.target)
    println(machine.name)
    try {
      val consumer = new CountGraphConsumer()
      val builder = new MultiCoGraphBuilder(machine, consumer)
      builder.buildCoGraph(task.target, null)
      consumer.showResults()
      println()
    } catch {
      case e: ModelingError => Console.println("ERR:" + e.message)
      println()
    }
  }

  def countPrograms(task: SLLTask, machine: SLLMachine) = {
    println(task.target)
    println(machine.name)
    try {
      val consumer = new CountProgramConsumer()
      val builder = new MultiCoGraphBuilder(machine, consumer)
      builder.buildCoGraph(task.target, null)
      consumer.showResults()
      println()
    } catch {
      case e: ModelingError => Console.println("ERR:" + e.message)
      println()
    }
  }
  
  def runBaseSuperCompiler(task: SLLTask, machine: SLLMachine) = {
    println(task.target)
    val builder = new SingleCoGraphBuilder(machine)
    val coGraph = builder.buildCoGraph(task.target, null)
    println(coGraph)

    val graph = Transformations.transpose(coGraph)
    println(graph)
    
    val (goal, p) = new SLLResiduator(graph).result
    println(goal)
    println(p)
  }
}