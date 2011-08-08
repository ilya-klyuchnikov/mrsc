package mrsc.pfp.sll.samples

import mrsc.pfp.sll._

object SLLTasks {

  val peanoProgram: Program =
    """
    gEq(S(x), y) = gEqS(y, x);
	gEq(Z(), y) = gEqZ(y);

	gEqZ(S(x)) = False();
    gEqZ(Z()) = True();
	  
    gEqS(S(y), x) = gEq(x, y);
    gEqS(Z(), x) = False();
    
    gAdd(S(x), y) = S(gAdd(x, y));
	gAdd(Z(), y) = y;
    
    gMult(S(x), y) = gAdd(y, gMult(x, y));
    gMult(Z(), y) = Z();

    gFib(S(x)) = gFib1(x);
    gFib(Z()) = S(Z());
	
    gFib1(S(x)) = gAdd(gFib(S(x)), gFib(x));
    gFib1(Z()) = S(Z());

    gFastFib(S(m), x, y) = gFastFib(m, y, gAdd(x, y));
    gFastFib(Z(), x, y) = x;
    
    gAddAcc(S(x), y) = gAddAcc(x, S(y));
    gAddAcc(Z(), y) = y;  

    gEven(S(x)) = gOdd(x);
    gEven(Z()) = True();
    
    gOdd(S(x)) = gEven(x);
    gOdd(Z()) = False();
    
    gOr(False(), x) = x;
    gOr(True(), x) = True();
	"""

  val listProgram: Program =
    """
    
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    gApp(Nil(), vs) = vs;
    
    gRev(Cons(x, xs))=gApp(gRev(xs), Cons(x, Nil()));
    gRev(Nil()) = Nil();

    gFRev(Cons(x, xs), ys) = gFRev(xs, Cons(x, ys));
    gFRev(Nil(), ys) = ys; 
    
    g1(Cons(a, b), y, z) = Cons(a, g1(b, y, z));
    g1(Nil(), y, z) = g2(y, z);
    
    g2(Cons(a, b), z) = Cons(a, g2(b, z));
    g2(Nil(), z) = z;
    
    gLast(Cons(x, xs)) = gLast(xs);
    gLast(Nil()) = Nil();

    gIdle(Cons(x, xs)) = gIdle(gIdle(xs));
    gIdle(Nil()) = Nil();
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
      "EqPlusa" -> SLLTask("gEq(gAdd(m, n), gAdd(p, m))", peanoProgram),
      "EqPlusb" -> SLLTask("gEq(gAdd(m, n), gAdd(n, q))", peanoProgram),
      "EqPlusc" -> SLLTask("gEq(gAdd(m, n), gAdd(p, q))", peanoProgram),
      
      "EqPlus1" -> SLLTask("gEq(gAdd(m, S(n)), gAdd(n, S(m)))", peanoProgram),
      "EqPlus1a" -> SLLTask("gEq(gAdd(m, S(p)), gAdd(n, S(m)))", peanoProgram),
      "EqPlus1b" -> SLLTask("gEq(gAdd(m, S(n)), gAdd(n, S(q)))", peanoProgram),
      "EqPlus1c" -> SLLTask("gEq(gAdd(m, S(p)), gAdd(n, S(q)))", peanoProgram),
      
      "OddEven" -> SLLTask("gOr(gEven(m), gOdd(m))", peanoProgram),
      "LastDouble" -> SLLTask("gLast(gApp(xs, xs))", listProgram),
      "App" -> SLLTask("gApp(xs, xs)", listProgram),
      "EvenMult" -> SLLTask("gEven(gMult(m, n))", peanoProgram),
      "EvenSqr" -> SLLTask("gEven(gMult(m, m))", peanoProgram),
      "Idle" -> SLLTask("gIdle(xs)", listProgram))
      
   val task1 = SLLTask("gFib(S(S(S(S(S(S(Z())))))))", peanoProgram)
   val task2 = SLLTask("gRev(Cons(A(), Cons(B(), Cons(C(), Nil()))))", listProgram)
}