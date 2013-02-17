// sample showing how named terms are parsed into nameless syntax
import scalaz._
import Scalaz._
import mrsc.pfp._
import NamelessShows._

def term(in: String): Term =
  PFPParsers().inputTerm(in)
def bindings(in: String) =
  PFPParsers().inputBindings(in)

// bound vars are represented via indexes
val in1 = """\x -> \y -> x y"""
term(in1).shows
// special syntax for free vars
val in2 = """<1>"""
term(in2).shows
// if var is absent in the current context, then it is free var (g)
val in3 = """g (\x -> x)"""
term(in3).shows
// bound vars in case expressions are represented via indexes
val in4 ="""case xs of { Nil() -> Nil(); Cons(a, b) -> Cons(a, b)}"""
term(in4).shows
// let-expression
val in5 = """let x = Nil() in let f = g in f x"""
term(in5).shows
// letrec is represented via let + fix
val in6 = """letrec h = h x in h"""
term(in6).shows
// example of definition in a program
val in7 =
  """a=\x->\y->case x of {Nil()->y; Cons(h, t)->Cons(h,(a t y))};"""
bindings(in7)("a").shows






