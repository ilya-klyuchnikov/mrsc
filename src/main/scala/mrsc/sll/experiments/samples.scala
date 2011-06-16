package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

// try all variants
class Multi1(val program: Program, val ordering: PartialOrdering[Expr])
  extends GenericMultiMachine[Expr, DriveInfo[Expr], Extra]
  with SLLSyntax
  with SLLMetaEvaluator
  with PruningDriving[Expr]
  with Folding[Expr]
  with Termination[Expr]
  with AlwaysCurrentGens[Expr]
  with NoTricks[Expr]

// generalize (in all possible ways) current configuration (when whistle blows) 
class Multi2(val program: Program, val ordering: PartialOrdering[Expr])
  extends GenericMultiMachine[Expr, DriveInfo[Expr], Extra]
  with SLLSyntax
  with SLLMetaEvaluator
  with PruningDriving[Expr]
  with Folding[Expr]
  with Termination[Expr]
  with CurrentGensOnWhistle[Expr]
  with NoTricks[Expr]

// generalize (in all possible ways) blamed configuration (when whistle blows)
class Multi3(val program: Program, val ordering: PartialOrdering[Expr])
  extends GenericMultiMachine[Expr, DriveInfo[Expr], Extra]
  with SLLSyntax
  with SLLMetaEvaluator
  with PruningDriving[Expr]
  with Folding[Expr]
  with Termination[Expr]
  with SLLWhistleBlamedGens
  with NoTricks[Expr]

// when whistle blows, it considers all generalization of two nodes:
// 1. the blamed one (with rollback)
// 2. the current one
class Multi4(val program: Program, val ordering: PartialOrdering[Expr])
  extends GenericMultiMachine[Expr, DriveInfo[Expr], Extra]
  with SLLSyntax
  with SLLMetaEvaluator
  with PruningDriving[Expr]
  with Folding[Expr]
  with Termination[Expr]
  with SLLWhistleAllGens
  with NoTricks[Expr]

// generalize (in all possible ways) blamed configuration (when whistle blows)
class Multi5(val program: Program, val ordering: PartialOrdering[Expr])
  extends GenericMultiMachine[Expr, DriveInfo[Expr], Extra]
  with SLLSyntax
  with SLLMetaEvaluator
  with PruningDriving[Expr]
  with Folding[Expr]
  with Termination[Expr]
  with SLLWhistleBlamedGens2
  with NoTricks[Expr]

class ClassicBlamedGen(val program: Program, val ordering: PartialOrdering[Expr])
  extends GenericMultiMachine[Expr, DriveInfo[Expr], Extra]
  with SLLSyntax
  with SLLMetaEvaluator
  with SimpleDriving[Expr]
  with Folding[Expr]
  with Termination[Expr]
  with SLLBlamedMsg
  with NoTricks[Expr]

class ClassicCurrentGen(val program: Program, val ordering: PartialOrdering[Expr])
  extends GenericMultiMachine[Expr, DriveInfo[Expr], Extra]
  with SLLSyntax
  with SLLMetaEvaluator
  with SimpleDriving[Expr]
  with Folding[Expr]
  with Termination[Expr]
  with SLLCurentMsg
  with NoTricks[Expr]

class ClassicMix(val program: Program, val ordering: PartialOrdering[Expr])
  extends GenericMultiMachine[Expr, DriveInfo[Expr], Extra]
  with SLLSyntax
  with SLLMetaEvaluator
  with SimpleDriving[Expr]
  with Folding[Expr]
  with Termination[Expr]
  with SLLMixMsg
  with NoTricks[Expr]

object Samples {
  type Machine1 = Machine[Expr, DriveInfo[Expr], Extra]

  def multi1(w: PartialOrdering[Expr])(p: Program) = new Multi1(p, w)
  def multi2(w: PartialOrdering[Expr])(p: Program) = new Multi2(p, w)
  def multi3(w: PartialOrdering[Expr])(p: Program) = new Multi3(p, w)
  def multi4(w: PartialOrdering[Expr])(p: Program) = new Multi4(p, w)
  def multi5(w: PartialOrdering[Expr])(p: Program) = new Multi5(p, w)
  def classic1(w: PartialOrdering[Expr])(p: Program) = new ClassicBlamedGen(p, w)
  def classic2(w: PartialOrdering[Expr])(p: Program) = new ClassicCurrentGen(p, w)
  def classic3(w: PartialOrdering[Expr])(p: Program) = new ClassicMix(p, w)

  // just tries classic variants of 
  // SLL supercompilation
  def classic(task: SLLTask): Unit = {

    
    val m1 = classic1(HEByCouplingWhistle)(task.program)
    val consumer1 = new SingleProgramConsumer()
    val builder1 = new CoGraphBuilder(m1, consumer1)
    builder1.buildCoGraph(task.target, NoExtra)
    println("**classic up:**")
    consumer1.showResults
    
    Checker.check(task, consumer1.residualTask)

    val m2 = classic2(HEByCouplingWhistle)(task.program)
    val consumer2 = new SingleProgramConsumer()
    val builder2 = new CoGraphBuilder(m2, consumer2)
    builder2.buildCoGraph(task.target, NoExtra)
    println("**classic down:**")
    consumer2.showResults
    
    Checker.check(task, consumer2.residualTask)
     
    

    val h1 = expand(40, task.target.toString)
    //print(h1)

    {
      val m3 = classic3(HEByCouplingWhistle)(task.program)
      val consumer3 = new CountProgramConsumer2()
      val builder3 = new CoGraphBuilder(m3, consumer3)
      builder3.buildCoGraph(task.target, NoExtra)
      consumer3.showResults

      val r1 = expandRight(5, consumer3.result)
      //print(r1)
      
      for (sllTask2 <- consumer3.sllTasks) {
       println(sllTask2)
       //println(SLLExpressions.pretty(sllTask2))
       println("***")
       val taskS = SLLExpressions.expr2Task(sllTask2)
       println(taskS)
       println("+++")
       Checker.check(task, taskS)
      }
      
    }

    {
      val m3 = classic3(HEByCouplingWithRedexWhistle)(task.program)
      val consumer3 = new CountProgramConsumer2()
      val builder3 = new CoGraphBuilder(m3, consumer3)
      builder3.buildCoGraph(task.target, NoExtra)
      consumer3.showResults

      val r1 = expandRight(5, consumer3.result)
      //print(r1)

      for (sllTask2 <- consumer3.sllTasks) {
       println(sllTask2)
       //println(SLLExpressions.pretty(sllTask2))
       println("***")
       val taskS = SLLExpressions.expr2Task(sllTask2)
       println(taskS)
       println("+++")
       Checker.check(task, taskS)
      }
    }

    println()
  }

  def report(task: SLLTask): Unit = {
    classic(task)
  }

  // instead of generating programs,
  // this pre-run just estimate the maximum number of programs
  // pre-run is not memory consuming, but potentially is time-consuming
  def preRunTask(task: SLLTask, f: Program => Machine1) = {
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

  def runTask(task: SLLTask, f: Program => Machine1) = {
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

  def simpleAnalysis(): Unit = {

    val header = expand(40, """Task \ Supercompiler""") + expandRight(5, "1") +
      expandRight(5, "2") + expandRight(5, "3")
    println(header)
    println()

    
    report(SLLTasks.namedTasks("NaiveFib"))
    report(SLLTasks.namedTasks("FastFib"))

    report(SLLTasks.namedTasks("EqPlus"))

    report(SLLTasks.namedTasks("EqPlusa"))
    report(SLLTasks.namedTasks("EqPlusb"))
    report(SLLTasks.namedTasks("EqPlusc"))

    report(SLLTasks.namedTasks("EqPlus1"))
    report(SLLTasks.namedTasks("EqPlus1a"))
    report(SLLTasks.namedTasks("EqPlus1b"))
    report(SLLTasks.namedTasks("EqPlus1c"))

    report(SLLTasks.namedTasks("OddEven"))
    report(SLLTasks.namedTasks("EvenMult"))
    report(SLLTasks.namedTasks("EvenSqr"))

    report(SLLTasks.namedTasks("NaiveReverse"))
    report(SLLTasks.namedTasks("FastReverse"))
    
    report(SLLTasks.namedTasks("LastDouble"))
    report(SLLTasks.namedTasks("App"))
    
    report(SLLTasks.namedTasks("Idle"))

    println()
    println("1 - classic msg mix, he by coupling")
    println("2 - classic msg mix, he by coupling with redex")
    println("3 - classic all mutial gens, he by coupling")
  }

  private def expand(n: Int, s: String): String = {
    val init = " " * n
    val tmp = s + init
    tmp take n
  }

  private def expandRight(n: Int, s: String): String = {
    val init = " " * n
    val tmp = init + s
    tmp takeRight n
  }

  def main(args: Array[String]): Unit = {

    runTask(SLLTasks.namedTasks("NaiveReverse"), multi5(HEByCouplingWhistle)_)
    runTask(SLLTasks.namedTasks("FastReverse"), multi5(HEByCouplingWhistle)_)
    runTask(SLLTasks.namedTasks("NaiveFib"), multi5(HEByCouplingWhistle)_)
    
    // 85726 completed graphs here:
    //runTask(SLLTasks.namedTasks("FastFib"), multi5(HEWhistle)_)
    
    // 0 results here (because only UP generalization is allowed)
    // runTask(SLLTasks.namedTasks("FastFib"), multi3(HEByCouplingWhistle)_)
    
    simpleAnalysis()

  }

}