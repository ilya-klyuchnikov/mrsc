// the most interesting case is how fix point is evaluated
import scalaz._
import Scalaz._
import mrsc.pfp._
import NamelessShows._

def term(in: String): Term =
  PFPParsers().inputTerm(in)

var t =
  term("""letrec nil = \xs -> case xs of { Nil() -> Nil(); Cons(y, ys) -> nil ys } in nil Cons(a, Nil())""")
println(t.shows)
do {
  t = CBNEval.lazyStep(t, Map())
  println("=> " + t.shows)
} while (!CBNEval.isLazyVal(t))
