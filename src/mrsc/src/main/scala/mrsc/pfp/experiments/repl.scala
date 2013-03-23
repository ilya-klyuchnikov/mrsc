package mrsc.pfp.experiments

import mrsc.core._
import mrsc.pfp._

object repl {

  import scalaz._
  import Scalaz._
  import NamelessShows._

  def graphs(file: String, sc: PFPSC): Iterator[SGraph[MetaTerm, Label]] = {
    val (_, task) = io.taskFromFile(file)
    val rules = sc(task.bindings)
    GraphGenerator(rules, task.goal, false)
  }

  def showGraphs(file: String, sc: PFPSC) =
    for {sGraph <- graphs(file, sc)} {
      val tGraph = Transformations.transpose(sGraph)
      Console.println(tGraph)
    }

  def showFoldings(file: String, sc: PFPSC) =
    graphs(file, sc).zipWithIndex.foreach { case (sGraph, i) =>
      Console.println("### graph %s ###".format(i + 1))
      sGraph.completeLeaves.foreach {
        case leaf@SNode(_, _, Some(ePath), _) =>
          val Some(embedded) = leaf.ancestors.find(_.sPath == ePath)
          Console.println(embedded.conf.shows)
          Console.println("<*>")
          Console.println(leaf.conf.shows)
          Console.println()
        case _ =>
      }
    }

  def showResiduals(file: String, sc: PFPSC) =
    graphs(file, sc).zipWithIndex.foreach { case (sGraph, i) =>
      val tGraph = Transformations.transpose(sGraph)
      Console.println(Residuator(tGraph).result.shows)
    }
}
