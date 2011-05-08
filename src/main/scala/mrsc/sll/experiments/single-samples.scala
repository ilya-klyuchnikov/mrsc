package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

object Sample1 extends App {
  type Machine = MultiResultMachine[Expr, SubStepInfo, Extra]

  def runTask(task: SLLTask, f: Program => Machine) = {
    try {
      val machine = f(task.program)
      val consumer = new SingleProgramConsumer()
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

  def classic1(w: Whistle)(p: Program) = new Classic1(p, w)
  def classic2(w: Whistle)(p: Program) = new Classic2(p, w)

  val scs = List(
    ("classic, current, he", classic1(HEWhistle)_),
    ("classic, current, he + redex", classic1(HEWithRedexWhistle)_),
    ("classic, current, coupling", classic1(HEByCouplingWhistle)_),
    ("classic, current, coupling + redex", classic1(HEByCouplingWithRedexWhistle)_),
    ("classic, blames, he", classic2(HEWhistle)_),
    ("classic, blamed, he + redex", classic2(HEWithRedexWhistle)_),
    ("classic, blamed, coupling", classic2(HEByCouplingWhistle)_),
    ("classic, blamed, coupling + redex", classic2(HEByCouplingWithRedexWhistle)_))

  runTasks(scs)
}

class Classic1(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, SLLSignal]
  with SLLSimpleDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLCurentMsg
  with SLLNoTricks

class Classic2(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, SLLSignal]
  with SLLSimpleDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLBlamedMsg
  with SLLNoTricks