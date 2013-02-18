import scalaz._
import Scalaz._
import mrsc.core._
import mrsc.pfp._
def deforest(bindings: GContext, goal: Term) = {
  case class Deforester(val gc: GContext) extends PFPRules
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
val goal1: Term = "app <1> <2>"
val deforested1 = deforest(bindings, goal1)
deforested1.shows(NamelessShows.TermShow)

val goal2: Term = "app (app <1> <2>) <3>"
val deforested2 = deforest(bindings, goal2)
deforested2.shows(NamelessShows.TermShow)

val goal3: Term = "app <1> (app <2> <3>)"
val deforested3 = deforest(bindings, goal3)
deforested3.shows(NamelessShows.TermShow)
