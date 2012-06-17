package mrsc.pfp

trait Homeomorphic {
  def he(t1: MetaTerm, t2: MetaTerm) = heByDiving(t1, t2) || heByCoupling(t1, t2)
  def heByDiving(t1: MetaTerm, t2: MetaTerm): Boolean
  def heByCoupling(t1: MetaTerm, t2: MetaTerm): Boolean
}

// the strongest version of embedding
object HE {

  def he(t1: MetaTerm, t2: MetaTerm) = heByDiving(t1, t2) || heByCoupling(t1, t2)

  def heByDiving(t1: MetaTerm, t2: MetaTerm): Boolean = t1 match {
    case rb: Rebuilding => false
    case _ => t2 match {
      case Ctr(_, args)     => args exists (he(t1, _))
      case Abs(body)        => he(t1, body)
      case App(a1: App, a2) => heByDiving(t1, a1) || he(t1, a2)
      case App(a1, a2)      => he(t1, a1) || he(t1, a2)
      case Let(v, in)       => he(t1, v) || he(t1, in)
      case Fix(body)        => he(t1, body)
      case Case(sel, bs)    => he(t1, sel) || bs.exists { case (_, t) => he(t1, t) }
      case _                => false
    }
  }

  def heByCoupling(t1: MetaTerm, t2: MetaTerm): Boolean = (t1, t2) match {
    case (FVar(_), FVar(_))               => true
    case (BVar(i1), BVar(i2))             => i1 == i2
    case (GVar(n1), GVar(n2))             => n1 == n2
    case (Abs(b1), Abs(b2))               => he(b1, b2)
    case (a1: App, a2: App)               => heByCoupling(a1.t1, a2.t1) && he(a1.t2, a2.t2)
    case (l1: Let, l2: Let)               => he(l1.v, l2.v) && he(l1.in, l2.in)
    case (Fix(f1), Fix(f2))               => he(f1, f2)
    case (Ctr(n1, args1), Ctr(n2, args2)) => n1 == n2 && (args1, args2).zipped.forall(he)
    case (Case(s1, bs1), Case(s2, bs2))   => he(s1, s2) && (bs1, bs2).zipped.forall { case ((p1, b1), (p2, b2)) => p1 == p2 && he(b1, b2) }
    case _                                => false
  }

}