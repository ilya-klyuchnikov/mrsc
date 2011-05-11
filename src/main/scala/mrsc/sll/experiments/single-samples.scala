package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

object Sample1 extends App {
  type Machine = MultiResultMachine[Expr, SubStepInfo[Expr], Extra]

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

  def classicBlamedGen(w: Whistle)(p: Program) = new ClassicBlamedGen(p, w)
  def classicCurrentGen(w: Whistle)(p: Program) = new ClassicCurrentGen(p, w)

  val scs = List(
    ("classic, current, he", classicBlamedGen(HEWhistle)_),
    ("classic, current, he + redex", classicBlamedGen(HEWithRedexWhistle)_),
    ("classic, current, coupling", classicBlamedGen(HEByCouplingWhistle)_),
    ("classic, current, coupling + redex", classicBlamedGen(HEByCouplingWithRedexWhistle)_),
    ("classic, blames, he", classicCurrentGen(HEWhistle)_),
    ("classic, blamed, he + redex", classicCurrentGen(HEWithRedexWhistle)_),
    ("classic, blamed, coupling", classicCurrentGen(HEByCouplingWhistle)_),
    ("classic, blamed, coupling + redex", classicCurrentGen(HEByCouplingWithRedexWhistle)_))

  runTasks(scs)
}

class ClassicBlamedGen(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo[Expr], Extra, SLLSignal]
  with SLLSimpleDriving
  with SLLFolding[SubStepInfo[Expr], Extra]
  with SLLWhistle
  with SLLCurentMsg
  with SLLNoTricks

class ClassicCurrentGen(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo[Expr], Extra, SLLSignal]
  with SLLSimpleDriving
  with SLLFolding[SubStepInfo[Expr], Extra]
  with SLLWhistle
  with SLLBlamedMsg
  with SLLNoTricks