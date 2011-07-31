package mrsc.sll

import mrsc._

object HE {
  def he_*(t1: Expr, t2: Expr): Boolean = he(t1, t2) && b(t1) == b(t2)

  def he(t1: Expr, t2: Expr) = (t1.size <= t2.size) && (heByDiving(t1, t2) || heByCoupling(t1, t2))

  private def heByDiving(t1: Expr, t2: Expr): Boolean = (t1.size < t2.size) && (t2 match {
    case Ctr(_, args) => args exists (he(t1, _))
    case FCall(_, args) => args exists (he(t1, _))
    case GCall(_, args) => args exists (he(t1, _))
    case _ => false
  })

  def heByCoupling(t1: Expr, t2: Expr): Boolean = (t1.size <= t2.size) && ((t1, t2) match {
    case (Ctr(n1, args1), Ctr(n2, args2)) => n1 == n2 && (args1, args2).zipped.forall(he)
    case (FCall(n1, args1), FCall(n2, args2)) => n1 == n2 && (args1, args2).zipped.forall(he)
    case (GCall(n1, args1), GCall(n2, args2)) => n1 == n2 && (args1, args2).zipped.forall(he)
    case (Var(_), Var(_)) => true
    case _ => false
  })

  def b(t: Expr): Int = t match {
    case GCall(_, args) => b(args.head)
    case Var(_) => 1
    case _ => 0
  }
}

object HEWhistle extends SimplePartialOrdering[Expr] {
  val name = "homeomorphic embedding"
  def lteq(e1: Expr, e2: Expr) = e2 match {
    case Let(_, _) => false
    case Ctr(_, _) => false
    case _ =>
      e1 match {
        case Let(_, _) => false
        case _ => HE.he(e1, e2)
      }
  }
}

object HEWithRedexWhistle extends SimplePartialOrdering[Expr] {
  val name = "homeomorphic embedding"
  def lteq(e1: Expr, e2: Expr) = e2 match {
    case Let(_, _) => false
    case Ctr(_, _) => false
    case _ =>
      e1 match {
        case Let(_, _) => false
        case _ => HE.he_*(e1, e2)
      }
  }
}

object HEByCouplingWhistle extends SimplePartialOrdering[Expr] {
  val name = "homeomorphic embedding"
  def lteq(e1: Expr, e2: Expr) = e2 match {
    case Let(_, _) => false
    case Ctr(_, _) => false
    case _ =>
      e1 match {
        case Let(_, _) => false
        case _ => HE.heByCoupling(e1, e2)
      }
  }
}

object HEByCouplingWithRedexWhistle extends SimplePartialOrdering[Expr] {
  val name = "homeomorphic embedding"
  def lteq(e1: Expr, e2: Expr) = e2 match {
    case Let(_, _) => false
    case Ctr(_, _) => false
    case _ =>
      e1 match {
        case Let(_, _) => false
        case _ => (HE.heByCoupling(e1, e2) && HE.b(e1) == HE.b(e2))
      }
  }
}