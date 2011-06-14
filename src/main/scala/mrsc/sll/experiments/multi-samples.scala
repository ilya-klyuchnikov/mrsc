package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

object Sample2 extends App {
  type Machine2 = Machine[Expr, DriveInfo[Expr], Extra]

  def runTask(task: SLLTask, f: Program => Machine2) = {
    try {
      val machine = f(task.program)
      val consumer = new CountProgramConsumer2()
      val builder = new CoGraphBuilder(machine, consumer)
      builder.buildCoGraph(task.target, NoExtra)
      consumer.showResults()
      println()
    } catch {
      case e: ModelingError =>
        Console.println("ERR:" + e.message)
        println()
    }
  }

  def runTask1(task: SLLTask, f: Program => Machine2) = {
    try {
      println(task.target)
      val machine = f(task.program)
      val consumer = new CountGraphConsumer[mrsc.sll.Expr, mrsc.DriveInfo[mrsc.sll.Expr], mrsc.Extra]()
      val builder = new CoGraphBuilder(machine, consumer)
      builder.buildCoGraph(task.target, NoExtra)
      consumer.showResults()
      println()
    } catch {
      case e: ModelingError =>
        Console.println("ERR:" + e.message)
        println()
    }
  }

  def runTasks(ins: List[(String, Program => Machine2)]) =
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
    //("classic, current, he", multi2(HEWhistle)_),
    //("classic, blamed, he", multi3(HEWhistle)_),
    ("classic, all, he", multi1(HEWhistle)_))

  //runTasks(scs)

  val t = SLLTasks.tasks(6)
  //runTask(t, multi1(Whistles.or(MaxGens(1), HEByCouplingWhistle))_)
  runTask1(t, multi1(Whistles.or(MaxLetParts(3), MaxGens(2), HEByCouplingWhistle))_)
}