import scalaz._
import Scalaz._
import mrsc.pfp._
import NamelessShows._

object PFP01Syntax extends scala.App {

  def showTerm(comment: String, t: MetaTerm): Unit = {
    println(s"== $comment== ")
    println(t)
    println(t.shows)
    println()
  }

  val globalVar = GVar("f")
  showTerm("Global variable", globalVar)

  // free variables are represented as positive numbers
  val freeVar = FVar(1)
  showTerm("Free Variable", freeVar)

  // bound variables are represented as de Bruijn indices
  // and are shown inside angle brackets
  val boundVar = BVar(1)
  showTerm("Bound Variable", boundVar)

  // lambda-abstraction
  val abstraction = Abs(BVar(0))
  showTerm("Abstraction", abstraction)

  val application = App(FVar(1), FVar(2))
  showTerm("Application", application)

  val constructor = Ctr("Cons", List(FVar(1), FVar(2)))
  showTerm("Constructor", constructor)

  // case-expression
  val caze = Case(FVar(1), (Ptr("Nil", List()) -> FVar(1)) :: (Ptr("Cons", List("a", "b")) -> BVar(2)) :: Nil)
  showTerm("Case expression", caze)

  // fix-point
  val fix = Fix(globalVar)
  showTerm("Simple fix-point", fix)

  // Let-expression
  val let = Let(FVar(1), BVar(0))
  showTerm("Let expression", let)

  // Rebuilding
  val rebuilding = Rebuilding(application, Map(FVar(1) -> FVar(3), FVar(2) -> FVar(3)))
  showTerm("Rebuilding", rebuilding)

}
