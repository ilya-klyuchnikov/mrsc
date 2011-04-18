package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

case class SLLTask(target: Expr, program: Program)

object SLLTasks {

  implicit def text2Program(s: String) = SLLParsers parseProg s
  implicit def text2Expr(s: String) = SLLParsers parseExpr s

  val peanoProgram: Program =
    """
	gEq(Z(), y) = gEqZ(y);
    gEq(S(x), y) = gEqS(y, x);

    gEqZ(Z()) = True();
    gEqZ(S(x)) = False();

    gEqS(Z(), x) = False();
    gEqS(S(y), x) = gEq(x, y);

    gAdd(Z(), y) = y;
    gAdd(S(x), y) = S(gAdd(x, y));

    gFib(Z()) = S(Z());
	gFib(S(x)) = gFib1(x);
    gFib1(Z()) = S(Z());
    gFib1(S(x)) = gAdd(gFib(S(x)), gFib(x));

    gAddAcc(Z(), y) = y;
    gAddAcc(S(x), y) = gAddAcc(x, S(y));
	"""

  val listProgram: Program =
    """
    gApp(Nil(), vs) = vs;
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    gRev(Nil()) = Nil();
    gRev(Cons(x, xs))=gApp(gRev(xs), Cons(x, Nil()));

    g1(Nil(), y, z) = g2(y, z);
    g1(Cons(a, b), y, z) = Cons(a, g1(b, y, z));
    g2(Nil(), z) = z;
    g2(Cons(a, b), z) = Cons(a, g2(b, z));
    """

  val tasks = List(
    //SLLTask("g1(x, y, z)", listProgram),
    SLLTask("gApp(x, y)", listProgram),
    SLLTask("gApp(gApp(x, y), z)", listProgram),
    SLLTask("gApp(gApp(x, y), y)", listProgram),
    SLLTask("gApp(gApp(x, x), z)", listProgram),
    SLLTask("gApp(gApp(x, y), x)", listProgram),
    SLLTask("gApp(gApp(x, x), x)", listProgram),
    SLLTask("gRev(x)", listProgram),
    SLLTask("gEq(x, x)", peanoProgram),
    SLLTask("gEq(gAdd(x, y), gAdd(y, x1))", peanoProgram),
    SLLTask("gFib(x)", peanoProgram),
    SLLTask("gFib(S(S(x)))", peanoProgram),
    SLLTask("gAddAcc(x, y)", peanoProgram),
    SLLTask("gEq(gAdd(a, b), gAdd(c, d))", peanoProgram),
    SLLTask("gEq(gAdd(a, S(b)), gAdd(c, S(d)))", peanoProgram))

  val namedTasks = Map(tasks map {t => (t.target.toString, t)}: _*)

}