package mrsc.pfp

import Syntax._

case class Rebuilding(t: Term, sub: Subst)

trait Rebuildings { self: VarGen =>

  def rebuildings(t: Term): List[Rebuilding] = rebuild(t, Map.empty)

  private def rebuild(e: Term, sub: Subst): List[Rebuilding] = {

    val rbs1: List[Rebuilding] = e match {
      case Abs(body) =>
        for { Rebuilding(t1, sub1) <- rebuild(body, sub) }
          yield Rebuilding(Abs(t1), sub1)
      case App(a1, a2) =>
        for {
          Rebuilding(t1, sub1) <- rebuild(a1, sub)
          Rebuilding(t2, sub2) <- rebuild(a2, sub1)
        } yield Rebuilding(App(t1, t2), sub2)
      case Let(a1, a2) =>
        for {
          Rebuilding(t1, sub1) <- rebuild(a1, sub)
          Rebuilding(t2, sub2) <- rebuild(a2, sub1)
        } yield Rebuilding(Let(t1, t2), sub2)
      case Fix(body) =>
        for { Rebuilding(t1, sub1) <- rebuild(body, sub) }
          yield Rebuilding(Fix(t1), sub1)
      case Ctr(n, xs) =>
        for { (ys, sub1) <- rebuild1(xs, sub) }
          yield Rebuilding(Ctr(n, ys), sub1)
      case Case(sel, bs) =>
        val (pts, bodies) = bs.unzip
        for {
          Rebuilding(sel1, sub1) <- rebuild(sel, sub)
          (bodies2, sub2) <- rebuild1(bodies, sub1)
        } yield Rebuilding(Case(sel1, pts zip bodies2), sub2)
      case _ =>
        List(Rebuilding(e, sub))
    }

    // extracting a term itself if it is extractable 
    val rbs2 =
      if (isFreeSubTerm(e)) {
        val fn = nextVar()
        List(Rebuilding(fn, sub + (fn -> e)))
      } else
        List()

    // term is already extracted
    val rbs3 = for { (k, e1) <- sub if e1 == e } yield Rebuilding(k, sub)

    rbs1 ++ rbs2 ++ rbs3
  }

  // all combinations of rebuildings a list of expressions 
  private def rebuild1(es: List[Term], sub: Subst): List[(List[Term], Subst)] =
    (es :\ ((List[Term](), sub) :: Nil)) { (e, acc) =>
      for { (es1, sub) <- acc; Rebuilding(t, sub1) <- rebuild(e, sub) } yield (t :: es1, sub1)
    }

}