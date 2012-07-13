package mrsc.pfp.samples

import mrsc.pfp._

case class Task(name: String, goal: Term, bindings: GContext)

object tasks {

  var tasks = Map[String, Task]()

  def apply(name: String): Task = tasks(name)

  task(
    "lam1",
    """\x -> x""",
    "")

  task(
    "app1", "app <1> <2>",

    """
    app = \x -> \y ->
      case x of {
        Nil()  -> y;
        Cons(x, xs) -> Cons(x, (app xs y))
      }; 
    """)

  // transforming supercompiled program!
  task(
    "app2",
    """
      letrec h = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, h xs y)
        } in h <1> <2>
      """,
    "")

  // repeated variable  
  task(
    "app3", "app <1> <1>",

    """
    app = \x -> \y ->
      case x of {
        Nil()  -> y;
        Cons(x, xs) -> Cons(x, (app xs y))
      }; 
    """)

  // generalizing supercompiled program!
  task(
    "app4",
    """
      letrec h = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, h xs y)
        } in h <1> <1>
      """,
    "")

  task(
    "fix1",
    """fix (\a -> A(a))""",
    """
    fix = \f -> f (fix f);
    """)

  task(
    "fix2",
    """fst(fix (\r -> P( A(snd r), B(fst r) )))""",
    """
    fst = \p -> case p of { P(x, y) -> x };
    snd = \p -> case p of { P(x, y) -> y };
    fix = \f -> f(fix f);
    """)

  task(
    "fix3",
    """fst (fix (\r -> t2 (A (snd r)) (B (fst r))))""",
    """
    t2 = \x -> \y -> \f -> f x y;
    fst = \f -> f (\x -> \y -> x);
    snd = \f -> f (\x -> \y -> y);
    fix = \f -> f(fix f);
    """)

  task(
    "iterate",
    """iterate (\n -> S(n)) <1>""",
    """
    iterate = \f -> \x -> Cons(x, (iterate f (f x)));
    """)

  task(
    "rev",
    """rev <1> """,
    """
    rev = \x ->
      case x of {
        Nil() -> Nil();
        Cons(x1, xs) -> app (rev xs) Cons(x1, Nil())
    };
    app = \x -> \y ->
      case x of {
        Nil()  -> y;
        Cons(x1, xs) -> Cons(x1, (app xs y))
      };
    """)

  task(
    "rev1",
    """case 
       case <5> of {Cons(p, q) -> app (rev q) Cons(p, Nil()); Nil() -> Nil()} 
       of { Cons(r, s) -> Cons(r, (app s Cons(<4>, Nil() ))); Nil() -> Cons(<4>, Nil())}
    """,
    """
    rev = \x ->
      case x of {
        Nil() -> Nil();
        Cons(x1, xs) -> app (rev xs) Cons(x1, Nil())
    };
    app = \x -> \y ->
      case x of {
        Nil()  -> y;
        Cons(x1, xs) -> Cons(x1, (app xs y))
      };
    """)

  private def task(name: String, goal: Term, bindings: GContext): Unit = {
    val t = Task(name, goal, bindings)
    tasks = tasks + (t.name -> t)
  }

}