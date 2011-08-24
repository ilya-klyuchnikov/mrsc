package mrsc.pfp.sll.samples

import scala.collection.immutable.TreeSet

import mrsc.core._
import mrsc.pfp._
import mrsc.pfp.sll._

case class ScResult(completed: Integer, unworkable: Integer, residuals: Set[Expr])

object Counting {

  implicit val exprOrdering: Ordering[Expr] = Ordering.by(_.size)

  def sc(gen: Iterator[Graph[Expr, DriveInfo[Expr], Extra[Expr]]], limit: Int): Either[ScResult, ScResult] = {
    var completed = 0
    var unworkable = 0
    var residuals = TreeSet[Expr]()
    for (g <- gen) {
      if (g.isUnworkable) {
        unworkable += 1
      } else {
        completed += 1
        val tg = Transformations.transpose(g)
        val expr = SLLResiduator.residuate(tg)
        residuals += expr
      }
      if (completed + unworkable > limit) {
        return Left(ScResult(completed, unworkable, residuals))
      }
    }
    Right(ScResult(completed, unworkable, residuals))
  }

  def compareScWithBinaryWhistle(task: SLLTask, whistle: PartialOrdering[Expr], limit: Int = 5000): Unit = {
    println()
    println("===== " + task.target + " ====")

    val machines = List(
      new ClassicCurrentGen(task.program, whistle),
      new MultiUpperAllBinaryGens(task.program, whistle),
      new MultiLowerAllBinaryGens(task.program, whistle),
      new MultiDoubleAllBinaryGens(task.program, whistle))
      //new MultiUpperRebuildings(task.program, whistle),
      //new MultiLowerRebuildings(task.program, whistle),
      //new MultiDoubleRebuildingsOnWhistle(task.program, whistle),
      //new MultiAllRebuildings(task.program, whistle))
      
    machines.foreach { m =>
      val gen = new GraphGenerator(m, task.target, NoExtra)
      val res = sc(gen, limit)
      res match {
        case Left(res) => println("- " + (res.completed, res.unworkable, res.residuals.size))
        case Right(res) => println("+ " + (res.completed, res.unworkable, res.residuals.size))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val tasks = List(
      SLLTasks.namedTasks("NaiveReverse"),
      SLLTasks.namedTasks("FastReverse"),
      SLLTasks.namedTasks("NaiveFib"),
      SLLTasks.namedTasks("FastFib"),
      SLLTasks.namedTasks("EqPlus"),
      SLLTasks.namedTasks("EqPlusa"),
      SLLTasks.namedTasks("EqPlusb"),
      SLLTasks.namedTasks("EqPlusc"),
      SLLTasks.namedTasks("EqPlus1"),
      SLLTasks.namedTasks("EqPlus1a"),
      SLLTasks.namedTasks("EqPlus1b"),
      SLLTasks.namedTasks("EqPlus1c"),
      SLLTasks.namedTasks("OddEven"),
      SLLTasks.namedTasks("LastDouble"),
      SLLTasks.namedTasks("App"),
      SLLTasks.namedTasks("EvenMult"),
      SLLTasks.namedTasks("EvenSqr"),
      SLLTasks.namedTasks("Idle")
      )

    
    tasks.foreach(compareScWithBinaryWhistle(_, HEWhistle, 100000))
    //tasks.foreach(compareScWithBinaryWhistle(_, HEWithRedexWhistle, 10000))
    //tasks.foreach(compareScWithBinaryWhistle(_, HEByCouplingWhistle, 15000))
    //tasks.foreach(compareScWithBinaryWhistle(_, HEByCouplingWithRedexWhistle, 15000))
  }
}
