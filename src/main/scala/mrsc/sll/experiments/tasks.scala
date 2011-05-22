package mrsc.sll.experiments

import mrsc._
import mrsc.sll._

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
    gMult(Z(), y) = Z();
    gMult(S(x), y) = gAdd(y, gMult(x, y));

    gFib(Z()) = S(Z());
	gFib(S(x)) = gFib1(x);
    gFib1(Z()) = S(Z());
    gFib1(S(x)) = gAdd(gFib(S(x)), gFib(x));

    gFastFib(Z(), x, y) = x;
    gFastFib(S(m), x, y) = gFastFib(m, y, gAdd(x, y));

    gAddAcc(Z(), y) = y;
    gAddAcc(S(x), y) = gAddAcc(x, S(y));

    gEven(Z()) = True();
    gEven(S(x)) = gOdd(x);
    gOdd(Z()) = False();
    gOdd(S(x)) = gEven(x);

    gOr(True(), x) = True();
    gOr(False(), x) = x;
	"""

  val listProgram: Program =
    """
    gApp(Nil(), vs) = vs;
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    gRev(Nil()) = Nil();
    gRev(Cons(x, xs))=gApp(gRev(xs), Cons(x, Nil()));

    gFRev(Nil(), ys) = ys;
    gFRev(Cons(x, xs), ys) = gFRev(xs, Cons(x, ys)); 
    
    g1(Nil(), y, z) = g2(y, z);
    g1(Cons(a, b), y, z) = Cons(a, g1(b, y, z));
    g2(Nil(), z) = z;
    g2(Cons(a, b), z) = Cons(a, g2(b, z));

    gLast(Nil()) = Nil();
    gLast(Cons(x, xs)) = gLast(xs);

    gIdle(Nil()) = Nil();
    gIdle(Cons(x, xs)) = gIdle(gIdle(xs));
    """

  val tasks = List(
    SLLTask("gApp(x, y)", listProgram),
    SLLTask("gApp(gApp(x, y), z)", listProgram),
    SLLTask("gApp(gApp(x, y), y)", listProgram),
    SLLTask("gApp(gApp(x, x), z)", listProgram),
    SLLTask("gApp(gApp(x, y), x)", listProgram),
    SLLTask("gApp(gApp(x, x), x)", listProgram),
    SLLTask("gRev(x)", listProgram),
    SLLTask("gApp(gApp(gRev(xs), Cons(y, Nil())), ys)", listProgram),
    SLLTask("gEq(x, x)", peanoProgram),
    SLLTask("gEq(gAdd(x, y), gAdd(y, x1))", peanoProgram),
    SLLTask("gFib(x)", peanoProgram),
    SLLTask("gFib(S(S(x)))", peanoProgram),
    SLLTask("gAddAcc(x, y)", peanoProgram),
    SLLTask("gEq(gAdd(a, b), gAdd(c, d))", peanoProgram),
    SLLTask("gEq(gAdd(a, S(b)), gAdd(c, S(d)))", peanoProgram),
    SLLTask("gOr(gEven(x), gOdd(x))", peanoProgram),
    SLLTask("gOr(gEven(x), gOdd(x))", peanoProgram))

  val namedTasks =
    Map(
      "NaiveReverse" -> SLLTask("gRev(xs)", listProgram),
      "FastReverse" -> SLLTask("gFRev(xs, Nil())", listProgram),
      "NaiveFib" -> SLLTask("gFib(m)", peanoProgram),
      "FastFib" -> SLLTask("gFastFib(m, S(Z()), S(Z()))", peanoProgram),
      "EqPlus" -> SLLTask("gEq(gAdd(m, n), gAdd(n, m))", peanoProgram),
      "OddEven" -> SLLTask("gOr(gEven(m), gOdd(m))", peanoProgram),
      "LastDouble" -> SLLTask("gLast(gApp(xs, xs))", listProgram),
      "EvenMult" -> SLLTask("gEven(gMult(m, n))", peanoProgram),
      "EvenSqr" -> SLLTask("gEven(gMult(m, m))", peanoProgram),
      "Idle" -> SLLTask("gIdle(xs)", listProgram))
      
   val task1 = SLLTask("gFib(S(S(S(S(S(S(Z())))))))", peanoProgram)
   val task2 = SLLTask("gRev(Cons(A(), Cons(B(), Cons(C(), Nil()))))", listProgram)
}