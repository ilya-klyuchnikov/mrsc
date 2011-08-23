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

// generalize (in all possible ways) a dangerous configuration (when whistle blows)
class Multi3(val program: Program, val ordering: PartialOrdering[Expr])
  extends PFPMachine[Expr]
  with SLLSyntax
  with SLLSemantics
  with Driving[Expr]
  with RenamingFolding[Expr]
  with BinaryWhistle[Expr]
  with LowerMsgOrUpperMggOnBinaryWhistle[Expr]

// when whistle blows, it considers all generalization of two nodes:
// 1. the dangerous one (with rollback)
// 2. the current one
class Multi4(val program: Program, val ordering: PartialOrdering[Expr])
  extends PFPMachine[Expr]
  with SLLSyntax
  with SLLSemantics
  with Driving[Expr]
  with RenamingFolding[Expr]
  with BinaryWhistle[Expr]
  with DoubleRebuildingsOnBinaryWhistle[Expr]

class ClassicDangerousGen(val program: Program, val ordering: PartialOrdering[Expr])
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
  def classic1(w: PartialOrdering[Expr])(p: Program) = new ClassicDangerousGen(p, w)
  def classic2(w: PartialOrdering[Expr])(p: Program) = new ClassicCurrentGen(p, w)
  def classic3(w: PartialOrdering[Expr])(p: Program) = new ClassicMix(p, w)

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

  private def residuateAndCheck(gen: GraphGenerator[Expr, DriveInfo[Expr], Extra[Expr]], task: SLLTask): Unit = {
    for (g <- gen if g.isComplete) {
      val t = Transformations.transpose(g)
      println(t)
      val res = SLLResiduator.residuate(t)
      println(PrettySLL.pretty(res))
      Checker.check(task, Lifting.expr2Task(res))
    }
  }

  // just tries classic variants of 
  // SLL supercompilation
  def showResidualPrograms(task: SLLTask): Unit = {

    println("************************")
    println(task.target)
    println("************************")
    println()

    {
      println("**classic+ up:**")
      val m1 = classic1(HEByCouplingWhistle)(task.program)
      residuateAndCheck(new GraphGenerator(m1, task.target, NoExtra), task)
    }

    {
      println("**classic+ down:**")
      val m2 = classic2(HEByCouplingWhistle)(task.program)
      residuateAndCheck(new GraphGenerator(m2, task.target, NoExtra), task)

    }

    println("**others:**")

    {
      val m3 = classic3(HEByCouplingWhistle)(task.program)
      residuateAndCheck(new GraphGenerator(m3, task.target, NoExtra), task)
    }

    {
      val m3 = classic3(HEByCouplingWithRedexWhistle)(task.program)
      residuateAndCheck(new GraphGenerator(m3, task.target, NoExtra), task)
    }

    println()
  }

  def showResidualProgramsForTasks(): Unit = {

    showResidualPrograms(SLLTasks.namedTasks("NaiveFib"))
    showResidualPrograms(SLLTasks.namedTasks("FastFib"))
    showResidualPrograms(SLLTasks.namedTasks("EqPlus"))
    showResidualPrograms(SLLTasks.namedTasks("EqPlusa"))
    showResidualPrograms(SLLTasks.namedTasks("EqPlusb"))
    showResidualPrograms(SLLTasks.namedTasks("EqPlusc"))
    showResidualPrograms(SLLTasks.namedTasks("EqPlus1"))
    showResidualPrograms(SLLTasks.namedTasks("EqPlus1a"))
    showResidualPrograms(SLLTasks.namedTasks("EqPlus1b"))
    showResidualPrograms(SLLTasks.namedTasks("EqPlus1c"))
    showResidualPrograms(SLLTasks.namedTasks("OddEven"))
    showResidualPrograms(SLLTasks.namedTasks("EvenMult"))
    showResidualPrograms(SLLTasks.namedTasks("EvenSqr"))
    showResidualPrograms(SLLTasks.namedTasks("NaiveReverse"))
    showResidualPrograms(SLLTasks.namedTasks("FastReverse"))
    showResidualPrograms(SLLTasks.namedTasks("LastDouble"))
    showResidualPrograms(SLLTasks.namedTasks("App"))
    showResidualPrograms(SLLTasks.namedTasks("Idle"))

  }

  // count graphs
  def count(gen: GraphGenerator[_, _, _], limit: Integer = 2000): (Integer, Integer) = {
    var completed = 0
    var unworkable = 0
    for (g <- gen) {
      if (g.isComplete) {
        completed += 1
      } else {
        unworkable += 1
      }
      if (completed > limit) {
        return (-1, -1)
      }
    }
    (completed, unworkable)
  }

  def countGraphs(task: SLLTask): Unit = {
    val info = expand(40, task.target.toString)
    print(info)

    val machines = List(
      new ClassicMix(task.program, HEByCouplingWhistle),
      new Multi4(task.program, HEWhistle),
      new Multi1(task.program, HEWhistle))

    machines foreach { m =>
      val gen = GraphGenerator(m, task.target, NoExtra)
      val (completed, unworkable) = count(gen)
      val res = expandRight(12, completed + "/" + unworkable)
      print(res)
    }

    println()
  }

  def countGraphsForTasks(): Unit = {
    println("Counting completed graphs")
    println()
    val header = expand(40, """Task \ Supercompiler""") + expandRight(12, "1") +
      expandRight(12, "2") + expandRight(12, "3")
    println(header)
    println()

    countGraphs(SLLTasks.namedTasks("NaiveFib"))
    countGraphs(SLLTasks.namedTasks("FastFib"))
    countGraphs(SLLTasks.namedTasks("EqPlus"))
    countGraphs(SLLTasks.namedTasks("EqPlusa"))
    countGraphs(SLLTasks.namedTasks("EqPlusb"))
    countGraphs(SLLTasks.namedTasks("EqPlusc"))
    countGraphs(SLLTasks.namedTasks("EqPlus1"))
    countGraphs(SLLTasks.namedTasks("EqPlus1a"))
    countGraphs(SLLTasks.namedTasks("EqPlus1b"))
    countGraphs(SLLTasks.namedTasks("EqPlus1c"))
    countGraphs(SLLTasks.namedTasks("OddEven"))
    countGraphs(SLLTasks.namedTasks("EvenMult"))
    countGraphs(SLLTasks.namedTasks("EvenSqr"))
    countGraphs(SLLTasks.namedTasks("NaiveReverse"))
    countGraphs(SLLTasks.namedTasks("FastReverse"))
    countGraphs(SLLTasks.namedTasks("LastDouble"))
    countGraphs(SLLTasks.namedTasks("App"))
    countGraphs(SLLTasks.namedTasks("Idle"))

    println()
    println("1 - classic msg mix, he by coupling")
    println("2 - all gens (up and down by whistle), he")
    println("3 - always gen, he")

    println()
    println()
  }

  def main(args: Array[String]): Unit = {

    // 85726 completed graphs here:
    //preRun(SLLTasks.namedTasks("FastFib"))

    // 0 results here (because only UP generalization is allowed)
    // runTask(SLLTasks.namedTasks("FastFib"), multi3(HEByCouplingWhistle)_)

    countGraphsForTasks()

    showResidualProgramsForTasks()

  }

}