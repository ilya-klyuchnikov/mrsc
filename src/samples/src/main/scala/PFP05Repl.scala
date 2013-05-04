import mrsc.pfp._
import misc._

// TODO: refactor to several examples
// TODO: refactor graph pretty-printing and serialization printing to use Show
// Right now this sample demonstrate several facilities:
// 1. It is easy to define "semi-supercompilers" that performs very simple analysis.
//    E.g.: EmbeddingsDetectors.SC2 performs folding when embedding is detected.
// 2. Facilities of Simple REPL (that runs supercompiler and outputs something from graph).
//    In this example it outputs expressions for which whistle blows.
// 3. An attempt to convert O(n^2) function into O(n) function
//    (see pfp/s001/fin.pfp). The whistle blows for two expressions
//    that cannot be folded. After supercompilation both of them they
//    can be folded. Moreover: this step introduces super-linear speedup.
object PFP05Repl extends scala.App {
  import scalaz._
  import Scalaz._
  import NamelessShows._

  Console.println("## Supercompiling by EmbeddingsDetectors.SC2")
  Console.println("## Graph")
  repl.showGraphs("pfp/s001/fin2.pfp", EmbeddingsDetectors.SC2)
  Console.println()

  Console.println("## Embeddings")
  repl.showFoldings("pfp/s001/fin2.pfp", EmbeddingsDetectors.SC2)
  Console.println()

  Console.println("## Supercompiling the pair of expressions")

  Console.println("### the top expression")
  val (_, t1) = io.taskFromFile("pfp/s001/fin2a.pfp")
  Console.println(t1.goal.shows)
  repl.showResiduals("pfp/s001/fin2a.pfp", mrsc.pfp.SC2, true)

  Console.println("### renaming of the bottom expression")
  val (_, t2) = io.taskFromFile("pfp/s001/fin2b.pfp")
  Console.println(t2.goal.shows)
  repl.showResiduals("pfp/s001/fin2b.pfp", mrsc.pfp.SC2, true)
}
