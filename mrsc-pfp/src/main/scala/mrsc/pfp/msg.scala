package mrsc.pfp

// TODO: move to syntax
trait MSG extends VarGen {
  import NamelessSyntax._

  case class TMSG(term: Term, dSub: List[(FVar, Term, Term)])

  private def termMSG(term1: Term, term2: Term): Option[Rebuilding] =
    for (gen <- generalize(term1, term2)) yield {
      val TMSG(gTerm, dSub) = simplify(gen)
      val s1 = dSub.map { case (v, e1, e2) => (v, e1) }
      Rebuilding(gTerm, Map(s1: _*))
    }

  def strictTermMSG(term1: Term, term2: Term): Option[Rebuilding] =
    termMSG(term1, term2) filter { rb => subclass.lt(term1, rb.t) }

  private def generalize(e1: Term, e2: Term): Option[TMSG] = ((e1, e2) match {

    case (t1, t2) if t1 == t2 =>
      Some(TMSG(e1, List()))

    case (Ctr(n1, args1, _), Ctr(n2, args2, _)) if n1 == n2 =>
      for ((genArgs, sub) <- merge(args1.lazyZip(args2).map { generalize }))
        yield TMSG(Ctr(n1, genArgs), sub)

    case (Abs(body1, _), Abs(body2, _)) =>
      for (TMSG(genBody, sub) <- generalize(body1, body2))
        yield TMSG(Abs(genBody), sub)

    case (App(h1, arg1, _), App(h2, arg2, _)) =>
      for {
        TMSG(genHead, sub1) <- generalize(h1, h2)
        TMSG(genArg, sub2) <- generalize(arg1, arg2)
      } yield TMSG(App(genHead, genArg), sub1 ++ sub2)

    case (Case(sel1, bs1, _), Case(sel2, bs2, _)) =>
      val samePatterns = (bs1 map (_._1.name)) == (bs2 map (_._1.name))
      if (samePatterns) {
        for {
          TMSG(selGen, sub0) <- generalize(sel1, sel2)
          (genBs, sub) <- merge((bs1.map(_._2) lazyZip bs2.map(_._2)).map { generalize })
        } yield TMSG(Case(selGen, bs1.map(_._1) zip genBs), sub0 ++ sub)
      } else {
        None
      }

    case (Let(v1, in1, _), Let(v2, in2, _)) =>
      for {
        TMSG(vGen, vSub) <- generalize(v1, v2)
        TMSG(inGen, inSub) <- generalize(v1, v2)
      } yield TMSG(Let(vGen, inGen), vSub ++ inSub)

    case (Fix(body1, _), Fix(body2, _)) =>
      for (TMSG(genBody, sub) <- generalize(body1, body2))
        yield TMSG(Abs(genBody), sub)

    case _ =>
      None
  }) orElse trivialGen(e1, e2)

  private def trivialGen(t1: Term, t2: Term): Option[TMSG] = {
    import NamelessSyntax._
    if (isFreeSubTerm(t1) && isFreeSubTerm(t2)) {
      val nv = nextVar()
      Some(TMSG(nv, List((nv, t1, t2))))
    } else {
      None
    }
  }

  private def simplify(gen2: TMSG): TMSG =
    gen2.dSub match {
      case Nil => gen2
      case (el @ (v, e1, e2)) :: els =>
        val TMSG(simpledTerm, simpledSub) = simplify(TMSG(gen2.term, els))
        val (same, other) = simpledSub partition { case (_, t1, t2) => e1 == t1 && e2 == t2 }
        val sub = Map(same map { case (v1, _, _) => (v1, v) }: _*)
        val term = applySubst(simpledTerm, sub)
        TMSG(term, el :: other)
    }

  private def merge(gs: List[(Option[TMSG])]): Option[(List[Term], List[(FVar, Term, Term)])] = gs match {
    case Nil            => Some(List(), List())
    case Some(g1) :: gs => for { (ts, sub) <- merge(gs) } yield ((g1.term) :: ts, g1.dSub ++ sub)
    case _              => None
  }
}
