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

  def multi1(w: Whistle)(p: Program) = new Multi1(p, w)
  def multi2(w: Whistle)(p: Program) = new Multi2(p, w)
  def multi3(w: Whistle)(p: Program) = new Multi3(p, w)

  val scs = List(
    ("classic, current, he", multi1(HEWhistle)_),
    ("classic, current, he", multi2(HEWhistle)_),
    ("classic, current, he", multi3(HEWhistle)_))

  runTasks(scs)
}

// try all variants
class Multi1(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLAlwaysCurrentGens
  with SLLNoTricks

// generalize (in all possible ways) current configuration (when whistle blows) 
class Multi2(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLWhistleCurrentGens
  with SLLNoTricks

// generalize (in all possible ways) blamed configuration (when whistle blows)
class Multi3(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLWhistleBlamedGens
  with SLLNoTricks