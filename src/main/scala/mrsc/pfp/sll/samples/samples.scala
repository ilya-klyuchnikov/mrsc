package mrsc.pfp.sll.samples

import mrsc.core._
import mrsc.pfp._
import mrsc.pfp.sll._

// try all variants
class Multi1(val program: Program, val ordering: PartialOrdering[Expr])
  extends PFPMachine[Expr]
  with SLLSyntax
  with SLLSemantics
  with Driving[Expr]
  with RenamingFolding[Expr]
  with BinaryWhistle[Expr]
  with AllRebuildings[Expr]

// generalize (in all possible ways) current configuration (when whistle blows) 
class Multi2(val program: Program, val ordering: PartialOrdering[Expr])
  extends PFPMachine[Expr]
  with SLLSyntax
  with SLLSemantics
  with Driving[Expr]
  with RenamingFolding[Expr]
  with BinaryWhistle[Expr]
  with LowerRebuildingsOnBinaryWhistle[Expr]

// generalize (in all possible ways) a dubious configuration (when whistle blows)
class Multi3(val program: Program, val ordering: PartialOrdering[Expr])
  extends PFPMachine[Expr]
  with SLLSyntax
  with SLLSemantics
  with Driving[Expr]
  with RenamingFolding[Expr]
  with BinaryWhistle[Expr]
  with LowerMsgOrUpperMggOnBinaryWhistle[Expr]

// when whistle blows, it considers all generalization of two nodes:
// 1. the dubious one (with rollback)
// 2. the current one
class Multi4(val program: Program, val ordering: PartialOrdering[Expr])
  extends PFPMachine[Expr]
  with SLLSyntax
  with SLLSemantics
  with Driving[Expr]
  with RenamingFolding[Expr]
  with BinaryWhistle[Expr]
  with DoubleRebuildingsOnBinaryWhistle[Expr]

class ClassicDubiousGen(val program: Program, val ordering: PartialOrdering[Expr])
  extends PFPMachine[Expr]
  with SLLSyntax
  with SLLSemantics
  with Driving[Expr]
  with RenamingFolding[Expr]
  with BinaryWhistle[Expr]
  with LowerMsgOrUpperMggOnBinaryWhistle[Expr]

class ClassicCurrentGen(val program: Program, val ordering: PartialOrdering[Expr])
  extends PFPMachine[Expr]
  with SLLSyntax
  with SLLSemantics
  with Driving[Expr]
  with RenamingFolding[Expr]
  with BinaryWhistle[Expr]
  with MSGCurrentOrDriving[Expr]

class ClassicMix(val program: Program, val ordering: PartialOrdering[Expr])
  extends PFPMachine[Expr]
  with SLLSyntax
  with SLLSemantics
  with Driving[Expr]
  with RenamingFolding[Expr]
  with BinaryWhistle[Expr]
  with DoubleMsgOnBinaryWhistle[Expr]

object Samples {
  type Machine1 = Machine[Expr, DriveInfo[Expr], Extra[Expr]]

  def multi1(w: PartialOrdering[Expr])(p: Program) = new Multi1(p, w)
  def multi2(w: PartialOrdering[Expr])(p: Program) = new Multi2(p, w)
  def multi3(w: PartialOrdering[Expr])(p: Program) = new Multi3(p, w)
  def multi4(w: PartialOrdering[Expr])(p: Program) = new Multi4(p, w)
  def classic1(w: PartialOrdering[Expr])(p: Program) = new ClassicDubiousGen(p, w)
  def classic2(w: PartialOrdering[Expr])(p: Program) = new ClassicCurrentGen(p, w)
  def classic3(w: PartialOrdering[Expr])(p: Program) = new ClassicMix(p, w)

  // just tries classic variants of 
  // SLL supercompilation
  def testRun(task: SLLTask): Unit = {

    println("************************")
    println(task.target)
    println("************************")
    println()
    
    {
      val m1 = classic1(HEByCouplingWhistle)(task.program)
      val consumer1 = new SingleProgramConsumer(SLLResiduator)
      val builder1 = new GraphBuilder(m1, consumer1)
      builder1.buildGraphs(task.target, NoExtra)
      println("**classic+ up:**")
      println(PrettySLL.pretty(consumer1.buildResult))

      Checker.check(task, Lifting.expr2Task(consumer1.residualProgram))
    }

    {
      val m2 = classic2(HEByCouplingWhistle)(task.program)
      val consumer2 = new SingleProgramConsumer(SLLResiduator)
      val builder2 = new GraphBuilder(m2, consumer2)
      builder2.buildGraphs(task.target, NoExtra)
      println("**classic+ down:**")
      println(PrettySLL.pretty(consumer2.buildResult))

      Checker.check(task, Lifting.expr2Task(consumer2.residualProgram))
    }

     println("**others:**")
     
    {
      val m3 = classic3(HEByCouplingWhistle)(task.program)
      val consumer3 = new ResiduatingConsumer(SLLResiduator)
      val builder3 = new GraphBuilder(m3, consumer3)
      builder3.buildGraphs(task.target, NoExtra)

      val ResidualResult(completed, discarded, residuals) = consumer3.buildResult

      for (sllTask2 <- residuals) {
        println(PrettySLL.pretty(sllTask2))
        println("***")
        val taskS = Lifting.expr2Task(sllTask2)
        println(taskS)
        println("+++")
        Checker.check(task, taskS)
      }

    }

    {
      val m3 = classic3(HEByCouplingWithRedexWhistle)(task.program)
      val consumer3 = new ResiduatingConsumer(SLLResiduator)
      val builder3 = new GraphBuilder(m3, consumer3)
      builder3.buildGraphs(task.target, NoExtra)

      val ResidualResult(completed, discarded, residuals) = consumer3.buildResult

      for (sllTask2 <- residuals) {
        println(PrettySLL.pretty(sllTask2))
        println("***")
        val taskS = Lifting.expr2Task(sllTask2)
        println(taskS)
        println("+++")
        Checker.check(task, taskS)
      }
    }

    println()
  }

  def preRunTasks(): Unit = {
    println("Counting completed graphs")
    println()
    val header = expand(40, """Task \ Supercompiler""") + expandRight(8, "1") +
      expandRight(8, "2") + expandRight(8, "3")
    println(header)
    println()
    
    preRun(SLLTasks.namedTasks("NaiveFib"))
    preRun(SLLTasks.namedTasks("FastFib"))
    preRun(SLLTasks.namedTasks("EqPlus"))
    preRun(SLLTasks.namedTasks("EqPlusa"))
    preRun(SLLTasks.namedTasks("EqPlusb"))
    preRun(SLLTasks.namedTasks("EqPlusc"))
    preRun(SLLTasks.namedTasks("EqPlus1"))
    preRun(SLLTasks.namedTasks("EqPlus1a"))
    preRun(SLLTasks.namedTasks("EqPlus1b"))
    preRun(SLLTasks.namedTasks("EqPlus1c"))
    preRun(SLLTasks.namedTasks("OddEven"))
    preRun(SLLTasks.namedTasks("EvenMult"))
    preRun(SLLTasks.namedTasks("EvenSqr"))
    preRun(SLLTasks.namedTasks("NaiveReverse"))
    preRun(SLLTasks.namedTasks("FastReverse"))
    preRun(SLLTasks.namedTasks("LastDouble"))
    preRun(SLLTasks.namedTasks("App"))
    preRun(SLLTasks.namedTasks("Idle"))
    
    println()
    println("1 - classic msg mix, he by coupling")
    println("2 - all gens (up and down by whistle), he")
    println("3 - always gen, he")
    
    println()
    println()
  }

  def preRun(task: SLLTask): Unit = {
    val info = expand(40, task.target.toString)
    print(info)

    {
      val machine = new ClassicMix(task.program, HEByCouplingWhistle)
      val consumer = new CountGraphConsumer[Expr, DriveInfo[Expr], Extra[Expr]]()
      val builder = new GraphBuilder(machine, consumer)
      try {
        builder.buildGraphs(task.target, NoExtra)
      } catch {
        case _ =>
      }
      val result = consumer.buildResult

      val res = expandRight(8, result.countCompleted + "")
      print(res)
    }
    
    {
      val machine = new Multi4(task.program, HEWhistle)
      val consumer = new CountGraphConsumer[Expr, DriveInfo[Expr], Extra[Expr]]()
      val builder = new GraphBuilder(machine, consumer)
      try {
        builder.buildGraphs(task.target, NoExtra)
      } catch {
        case _ =>
      }
      val result = consumer.buildResult

      val res = expandRight(8, result.countCompleted + "")
      print(res)
    }
    
    {
      val machine = new Multi1(task.program, HEWhistle)
      val consumer = new CountGraphConsumer[Expr, DriveInfo[Expr], Extra[Expr]]()
      val builder = new GraphBuilder(machine, consumer)
      try {
        builder.buildGraphs(task.target, NoExtra)
      } catch {
        case _ =>
      }
      val result = consumer.buildResult

      val res = expandRight(8, result.countCompleted + "")
      print(res)
    }

    println()
  }

  def testRunTasks(): Unit = {

    testRun(SLLTasks.namedTasks("NaiveFib"))
    testRun(SLLTasks.namedTasks("FastFib"))
    testRun(SLLTasks.namedTasks("EqPlus"))
    testRun(SLLTasks.namedTasks("EqPlusa"))
    testRun(SLLTasks.namedTasks("EqPlusb"))
    testRun(SLLTasks.namedTasks("EqPlusc"))
    testRun(SLLTasks.namedTasks("EqPlus1"))
    testRun(SLLTasks.namedTasks("EqPlus1a"))
    testRun(SLLTasks.namedTasks("EqPlus1b"))
    testRun(SLLTasks.namedTasks("EqPlus1c"))
    testRun(SLLTasks.namedTasks("OddEven"))
    testRun(SLLTasks.namedTasks("EvenMult"))
    testRun(SLLTasks.namedTasks("EvenSqr"))
    testRun(SLLTasks.namedTasks("NaiveReverse"))
    testRun(SLLTasks.namedTasks("FastReverse"))
    testRun(SLLTasks.namedTasks("LastDouble"))
    testRun(SLLTasks.namedTasks("App"))
    testRun(SLLTasks.namedTasks("Idle"))

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

    // 85726 completed graphs here:
    //preRun(SLLTasks.namedTasks("FastFib"))

    // 0 results here (because only UP generalization is allowed)
    // runTask(SLLTasks.namedTasks("FastFib"), multi3(HEByCouplingWhistle)_)
    
    preRunTasks()
    
    testRunTasks()

  }

}