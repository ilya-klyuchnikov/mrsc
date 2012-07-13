package mrsc.pfp.samples

import mrsc.core._
import mrsc.pfp._
import scala.annotation.tailrec

// REPL for interactive experiments with
// single-result supercompilers
object MRSC {

  def run(f: String, sc: SC) {
    
    val task = PFPParsers().taskFromFile(f)
    Console.println(task.name)
    Console.println(task.goal)

    val rules = sc(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, false)

    var count = 0
    var uniques: Set[Term] = Set()
    for { sGraph <- graphs } {
      val tGraph = Transformations.transpose(sGraph)
      val result = Residuator(tGraph).result

      //Console.println(tGraph.toString())
      //Console.println(result.toString())
      //Console.println(NamedSyntax.named(result))
      count += 1
      uniques = uniques + result
      Console.println("%s/%s".format(count, uniques.size))
    }

    val results = uniques.toList.sortBy(_.size)
    for {res <- results} {
      Console.println(res)
      Console.println(NamedSyntax.named(res))
    }
    
  }

  
}
