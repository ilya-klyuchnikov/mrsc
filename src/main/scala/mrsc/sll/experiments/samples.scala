package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

// try all variants
class Multi1(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo[Expr], Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo[Expr], Extra]
  with SLLWhistle
  with SLLAlwaysCurrentGens
  with SLLNoTricks

// generalize (in all possible ways) current configuration (when whistle blows) 
class Multi2(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo[Expr], Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo[Expr], Extra]
  with SLLWhistle
  with SLLWhistleCurrentGens
  with SLLNoTricks

// generalize (in all possible ways) blamed configuration (when whistle blows)
class Multi3(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo[Expr], Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo[Expr], Extra]
  with SLLWhistle
  with SLLWhistleBlamedGens
  with SLLNoTricks

// when whistle blows, it considers all generalization of two nodes:
// 1. the blamed one (with rollback)
// 2. the current one
class Multi4(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo[Expr], Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo[Expr], Extra]
  with SLLWhistle
  with SLLWhistleAllGens
  with SLLNoTricks

// generalize (in all possible ways) blamed configuration (when whistle blows)
class Multi5(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo[Expr], Extra, SLLSignal]
  with SLLPruningDriving
  with SLLFolding[SubStepInfo[Expr], Extra]
  with SLLWhistle
  with SLLWhistleBlamedGens2
  with SLLNoTricks

class ClassicBlamedGen(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo[Expr], Extra, SLLSignal]
  with SLLSimpleDriving
  with SLLFolding[SubStepInfo[Expr], Extra]
  with SLLWhistle
  with SLLBlamedMsg
  with SLLNoTricks

class ClassicCurrentGen(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo[Expr], Extra, SLLSignal]
  with SLLSimpleDriving
  with SLLFolding[SubStepInfo[Expr], Extra]
  with SLLWhistle
  with SLLCurentMsg
  with SLLNoTricks

class ClassicMix(val program: Program, val whistle: Whistle)
  extends GenericMultiMachine[Expr, SubStepInfo[Expr], Extra, SLLSignal]
  with SLLSimpleDriving
  with SLLFolding[SubStepInfo[Expr], Extra]
  with SLLWhistle
  with SLLMixMsg
  with SLLNoTricks

object Samples {
  type Machine = MultiResultMachine[Expr, SubStepInfo[Expr], Extra]

  def multi1(w: Whistle)(p: Program) = new Multi1(p, w)
  def multi2(w: Whistle)(p: Program) = new Multi2(p, w)
  def multi3(w: Whistle)(p: Program) = new Multi3(p, w)
  def multi4(w: Whistle)(p: Program) = new Multi4(p, w)
  def multi5(w: Whistle)(p: Program) = new Multi5(p, w)
  def classic1(w: Whistle)(p: Program) = new ClassicBlamedGen(p, w)
  def classic2(w: Whistle)(p: Program) = new ClassicCurrentGen(p, w)
  def classic3(w: Whistle)(p: Program) = new ClassicMix(p, w)

  // just tries classic variants of 
  // SLL supercompilation
  def classic(task: SLLTask): Unit = {
    println()
    println("--------")
    println(task.target)

    val m1 = classic1(HEByCouplingWhistle)(task.program)
    val consumer1 = new SingleProgramConsumer()
    val builder1 = new MultiCoGraphBuilder(m1, consumer1)
    builder1.buildCoGraph(task.target, DummyExtra)
    println("**classic up:**")
    consumer1.showResults
    
    Checker.check(task, consumer1.residualTask)

    val m2 = classic2(HEByCouplingWhistle)(task.program)
    val consumer2 = new SingleProgramConsumer()
    val builder2 = new MultiCoGraphBuilder(m2, consumer2)
    builder2.buildCoGraph(task.target, DummyExtra)
    println("**classic down:**")
    consumer2.showResults
    Checker.check(task, consumer2.residualTask)
    
    val m3 = classic3(HEByCouplingWhistle)(task.program)
    val consumer3 = new CountProgramConsumer2()
    val builder3 = new MultiCoGraphBuilder(m3, consumer3)
    builder3.buildCoGraph(task.target, DummyExtra)
    println("**classic mix:**")
    consumer3.showResults
    
    for (sllTask2 <- consumer3.sllTasks) {
      Checker.check(task, sllTask2)
    }
    
    println()
  }

  def report(task: SLLTask): Unit = {
    classic(task)
  }

  // instead of generating programs,
  // this pre-run just estimate the maximum number of programs
  // pre-run is not memory consuming, but potentially is time-consuming
  def preRunTask(task: SLLTask, f: Program => Machine) = {
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

  def simpleAnalysis(): Unit = {
    report(SLLTasks.namedTasks("NaiveFib"))
    report(SLLTasks.namedTasks("FastFib"))
    report(SLLTasks.namedTasks("EqPlus"))
    report(SLLTasks.namedTasks("OddEven"))
    report(SLLTasks.namedTasks("EvenMult"))
    report(SLLTasks.namedTasks("EvenSqr"))
    
    report(SLLTasks.namedTasks("NaiveReverse"))
    report(SLLTasks.namedTasks("FastReverse"))
    report(SLLTasks.namedTasks("LastDouble"))
    report(SLLTasks.namedTasks("Idle"))
  }

  def main(args: Array[String]): Unit = {

    //runTask(SLLTasks.namedTasks("Naive Reverse"), multi5(HEByCouplingWhistle)_)
    //runTask(SLLTasks.namedTasks("Fast Reverse"), multi5(HEByCouplingWhistle)_)
    //runTask(SLLTasks.namedTasks("Naive Fib"), multi5(HEByCouplingWhistle)_)
    //runTask(SLLTasks.namedTasks("Fast Fib"), multi5(HEByCouplingWhistle)_)
    //runTask(SLLTasks.namedTasks("Fast Fib"), multi3(HEByCouplingWhistle)_)

    simpleAnalysis()

  }
}