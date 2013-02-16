// simple example of nameless syntax

import mrsc.pfp._
import scalaz._
import Scalaz._

import NamelessShows._

// free variables are represented as positive numbers
val freeVar = FVar(1)
freeVar.shows

// bound variables are represented as de Bruijn indices
// and are shown inside angle brackets
val boundVar = BVar(1)
boundVar.shows
// global variables are represented
val globalVar = GVar("f")
globalVar.shows

// lambda-abstraction
val abstraction = Abs(BVar(0))
abstraction.shows

// application
val application = App(FVar(1), FVar(2))
application.shows

// constructor
val constructor = Ctr("Cons", List(FVar(1), FVar(2)))
constructor.shows

// Case-expression
val caze = Case(FVar(1),
  (Ptr("Nil", List()) -> FVar(1)) :: (Ptr("Cons", List("a", "b")) -> BVar(2)) :: Nil)

caze.shows

// Fix-point
val fix = Fix("f")
fix.shows
// Let-expression
val let = Let(FVar(1), BVar(0))
let.shows
// Rebuilding
val rebuilding = Rebuilding(application, Map(FVar(1) -> FVar(3), FVar(2) -> FVar(3)))


rebuilding.shows
