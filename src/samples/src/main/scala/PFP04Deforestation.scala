import scalaz._
import Scalaz._
import mrsc.core._
import mrsc.pfp._

// 2nd and 3rd transformations prove equivalence
object PFP04Deforestation extends scala.App {

  def deforest(bindings: GContext, goal: Term) = {
    case class Deforester(val gc: GContext)
      extends PFPRules
      with PFPSemantics
      with Driving
      with AllFoldingCandidates
      with Folding
      with NoWhistle
      with NoRebuildings

    val rules = new Deforester(bindings)
    val g = GraphGenerator(rules, goal).toList.head
    Residuator(Transformations.transpose(g)).result
  }

  val bindings: GContext =
    """
      app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, (app xs y))
        };
    """



  def sample1 {
    val goal1: Term = "app <1> <2>"
    println("   " + goal1.shows(NamelessShows.TermShow))
    val deforested1 = deforest(bindings, goal1)
    println("=> " + deforested1.shows(NamelessShows.TermShow))
    println()
  }

  def sample2 {
    val goal2: Term = "app (app <1> <2>) <3>"
    println("   " + goal2.shows(NamelessShows.TermShow))
    val deforested2 = deforest(bindings, goal2)
    println("=> " + deforested2.shows(NamelessShows.TermShow))
    println()
  }

  def sample3 {
    val goal3: Term = "app <1> (app <2> <3>)"
    println("   " + goal3.shows(NamelessShows.TermShow))
    val deforested3 = deforest(bindings, goal3)
    println("=> " + deforested3.shows(NamelessShows.TermShow))
    println()
  }

  sample1
  sample2
  sample3

}
