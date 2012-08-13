package mrsc.pfp.samples

import mrsc.pfp._
import mrsc.pfp.experiments._


object snippets {

  def s001() {
    Console.println("## Supercompiling by SC2")
    Console.println("## Graph")
    repl.showGraphs("pfp/s001/fin2.pfp", EmbeddingsDetectors.SC2)
    Console.println()

    Console.println("## Embeddings")
    repl.showFoldings("pfp/s001/fin2.pfp", EmbeddingsDetectors.SC2)
    Console.println()

    Console.println("## Supercompiling the pair of expressions")
    Console.println("### the top expression")
    val (_, t1) = io.taskFromFile("pfp/s001/fin2a.pfp")
    Console.println(t1.goal)
    repl.showResiduals("pfp/s001/fin2a.pfp", SC2)

    Console.println("### renaming of the bottom expression")
    val (_, t2) = io.taskFromFile("pfp/s001/fin2b.pfp")
    Console.println(t2.goal)
    repl.showResiduals("pfp/s001/fin2b.pfp", SC2)
  }

}
