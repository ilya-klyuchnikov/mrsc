package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

object Sample2 extends App {
  type Machine = MultiResultMachine[Expr, SubStepInfo, Extra]

  def runTask(task: SLLTask, f: Program => Machine) = {
    try {
      val machine = f(task.program)
      val consumer = new CountProgramConsumer2()
      val builder = new MultiCoGraphBuilder(machine, consumer)
      builder.buildCoGraph(task.target, DummyExtra)
      consumer.showResults()
      println()
    } catch {
      case e: ModelingError =>
        Console.println("ERR:" + e.message)
        println()
    }
  }

  def runTasks(ins: List[(String, Program => Machine)]) =
    for (t <- SLLTasks.tasks) {
      println("*****")
      println(t.target)
      println()
      for ((name, f) <- ins) {
        println(name)
        runTask(t, f)
      }
    }

  def classic1(w: Whistle)(p: Program) = new Classic11(p, w)
  def classic2(w: Whistle)(p: Program) = new Classic12(p, w)
  def classic3(w: Whistle)(p: Program) = new Classic13(p, w)

  val scs = List(
    ("classic, current, he", classic1(HEWhistle)_),
    ("classic, current, he", classic2(HEWhistle)_),
    ("classic, current, he", classic3(HEWhistle)_))

  runTasks(scs)
}

class Classic11(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLAlwaysCurrentGens
  with SLLNoTricks

class Classic12(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLWhistleCurrentGens
  with SLLNoTricks

class Classic13(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLWhistleBlamedGens
  with SLLNoTricks