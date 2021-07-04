import scalaz._
import Scalaz._
import mrsc.pfp._
import NamelessShows._

object PFP02Parsing extends scala.App {
  def term(in: String): Term =
    PFPParsers().inputTerm(in)
  def bindings(in: String) =
    PFPParsers().inputBindings(in)

  def showTermParsing(comment: String, in: String): Unit = {
    println(comment)
    println(in)
    println(term(in).shows)
    println()
  }

  def showBindingParsing(comment: String, in: String, fname: String): Unit = {
    println(comment)
    println(in)
    val bs = bindings(in)
    println(bs(fname).shows)
    println()
  }

  showTermParsing(
    "bound vars are represented via indexes",
    """\x -> \y -> x y""",
  )

  showTermParsing(
    "special syntax for free vars",
    """<1>""",
  )

  showTermParsing(
    "if var is absent in the current context, then it is free var (g)",
    """g (\x -> x)""",
  )

  showTermParsing(
    "bound vars in case expressions are represented via indexes",
    """case xs of { Nil() -> Nil(); Cons(a, b) -> Cons(a, b)}""",
  )

  showTermParsing(
    "let expression",
    """let x = Nil() in let f = g in f x""",
  )

  showTermParsing(
    "letrec is represented via let + fix",
    """letrec h = h x in h""",
  )

  showBindingParsing(
    "example of definition in a program",
    """a=\x->\y->case x of {Nil()->y; Cons(h, t)->Cons(h,(a t y))};""",
    "a",
  )
}
