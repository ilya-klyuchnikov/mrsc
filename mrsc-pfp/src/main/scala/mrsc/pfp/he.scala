package mrsc.pfp

// TODO: move into syntax
// The notion of homeomorphic embedding.
trait HE {
  def he(t1: MetaTerm, t2: MetaTerm) = heByDiving(t1, t2) || heByCoupling(t1, t2)
  def heByDiving(t1: MetaTerm, t2: MetaTerm): Boolean
  def heByCoupling(t1: MetaTerm, t2: MetaTerm): Boolean
}

// The strongest version of embedding.
// Refined for Hindley-Milner.
// The differences are specially marked below.
object HE3 extends HE {

  def heByDiving(t1: MetaTerm, t2: MetaTerm): Boolean = t1 match {
    case rb: Rebuilding => false
    // The difference
    case BVar(_, _) => false
    case _ => t2 match {
      case Ctr(_, args, _)     => args exists (he(t1, _))
      case Abs(body, _)        => he(t1, body)
      // The difference
      case App(a1: App, a2, _) => heByDiving(t1, a1) || he(t1, a2)
      case App(a1, a2, _)      => he(t1, a1) || he(t1, a2)
      case Let(v, in, _)       => he(t1, v) || he(t1, in)
      case Fix(body, _)        => he(t1, body)
      case Case(sel, bs, _)    => he(t1, sel) || bs.exists { case (_, t) => he(t1, t) }
      case _                   => false
    }
  }

  def heByCoupling(t1: MetaTerm, t2: MetaTerm): Boolean = (t1, t2) match {
    case (FVar(_, _), FVar(_, _))               => true
    case (BVar(i1, _), BVar(i2, _))             => i1 == i2
    case (GVar(n1, _), GVar(n2, _))             => n1 == n2
    case (Abs(b1, _), Abs(b2, _))               => he(b1, b2)
    // The difference
    case (a1: App, a2: App)                     => heByCoupling(a1.t1, a2.t1) && he(a1.t2, a2.t2)
    case (l1: Let, l2: Let)                     => he(l1.v, l2.v) && he(l1.in, l2.in)
    case (Fix(f1, _), Fix(f2, _))               => he(f1, f2)
    case (Ctr(n1, args1, _), Ctr(n2, args2, _)) => n1 == n2 && (args1, args2).zipped.forall(he)
    case (Case(s1, bs1, _), Case(s2, bs2, _))   => he(s1, s2) && (bs1, bs2).zipped.forall { case ((p1, b1), (p2, b2)) => p1 == p2 && he(b1, b2) }
    case _                                      => false
  }

}

object HE3Ordering extends SimplePartialOrdering[MetaTerm] {
  override def lteq(t1: MetaTerm, t2: MetaTerm) = HE3.he(t1, t2)
}

object HE3ByCouplingOrdering extends SimplePartialOrdering[MetaTerm] {
  override def lteq(t1: MetaTerm, t2: MetaTerm) = HE3.heByCoupling(t1, t2)
}

