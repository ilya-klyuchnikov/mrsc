package mrsc.sll

import mrsc._

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
