import scalaz._
import Scalaz._
import mrsc.pfp._
import NamelessShows._

object PFP03EvalStep extends scala.App {
  def term(in: String): Term =
    PFPParsers().inputTerm(in)

  def sample1(): Unit = {
    var t = term("""letrec nil = \xs -> case xs of { Nil() -> Nil(); Cons(y, ys) -> nil ys } in nil Cons(a, Nil())""")
    println("   " + t.shows)
    do {
      t = CBNEval.lazyStep(t, Map())
      println("=> " + t.shows)
    } while (!CBNEval.isLazyVal(t))
  }

  sample1()
}
