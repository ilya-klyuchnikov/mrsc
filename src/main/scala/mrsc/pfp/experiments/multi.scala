package mrsc.pfp.experiments

import mrsc.core._
import mrsc.pfp._

// For performing different experiments with multi-result supercompilers
object multi {

  case class AllMSC1(val gc: GContext, val maxGraphSize: Int) extends PFPRules
    with PFPSemantics
    with Driving
    with AllFoldingCandidates
    with Folding
    with AllEmbeddingCandidates
    with NoWhistle
    with AllRebuildings
    with SizeGraphFilter

  case class AllMSC2(val gc: GContext, val maxGraphSize: Int) extends PFPRules
    with PFPSemantics
    with LetDriving
    with AllFoldingCandidates
    with Folding
    with AllEmbeddingCandidates
    with NoWhistle
    with AllRebuildings
    with SizeGraphFilter

  def allDepthBound1(depth: Int): PFPSC = gc => AllMSC1(gc, depth)

  def allDepthBound2(depth: Int): PFPSC = gc => AllMSC2(gc, depth)

  // shows all possible variants of supercompilation of a given program
  def run(f: String, sc: PFPSC) {
    import scala.io.Source
    val text = Source.fromFile(f).mkString
    val task = PFPParsers().inputTask(text)
    Console.println(text)
    Console.println(task.goal)

    val rules = sc(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, false)

    var count = 0
    var uniques: Set[Term] = Set()
    for { sGraph <- graphs } {
      val tGraph = Transformations.transpose(sGraph)
      val result = Residuator1(tGraph).result
      count += 1
      uniques = uniques + result
      //Console.println("%s/%s".format(count, uniques.size))
      //Console.println(tGraph)
      //Console.println(NamedSyntax.named(result))
      //Console.println()
    }

    val results = uniques.toList.sortBy(_.size)
    for { res <- results } {
      Console.println(res)
      Console.println(NamedSyntax.named(res))
    }
  }

}
