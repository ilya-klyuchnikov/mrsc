package mrsc.sll

import mrsc._

trait Whistle {
  def name: String
  def accept(ps: PState[Expr, Contraction]): Boolean
}

case class ExpressionSize(size: Int) extends Whistle {
  val name = "size < " + size
  def accept(ps: PState[Expr, Contraction]) = ps.node.configuration match {
    case Let(_, _) => true
    case Ctr(_, _) => true
    case x => x.size < size
  }
}

object HEWhistle extends Whistle {
  val name = "homeomorphic embedding"
  def accept(ps: PState[Expr, Contraction]) = ps.node.configuration match {
    case Let(_, _) => true
    case Ctr(_, _) => true
    case x => ps.node.ancestors.forall { a =>
      a.configuration match {
        case Let(_, _) => true
        case ae => !HE.he(ae, x)
      }
    }
  }
}

object HEWithRedexWhistle extends Whistle {
  val name = "homeomorphic embedding with redex"
  def accept(ps: PState[Expr, Contraction]) = ps.node.configuration match {
    case Let(_, _) => true
    case Ctr(_, _) => true
    case x => ps.node.ancestors.forall { a =>
      a.configuration match {
        case Let(_, _) => true
        case ae => !HE.he_*(ae, x)
      }
    }
  }
}

object HEByCouplingWhistle extends Whistle {
  val name = "homeomorphic embedding via coupling"
  def accept(ps: PState[Expr, Contraction]) = ps.node.configuration match {
    case Let(_, _) => true
    case Ctr(_, _) => true
    case x => ps.node.ancestors.forall { a =>
      a.configuration match {
        case Let(_, _) => true
        case ae => !HE.heByCoupling(ae, x)
      }
    }
  }
}

object HEByCouplingWithRedexWhistle extends Whistle {
  val name = "homeomorphic embedding via coupling with redex"
  def accept(ps: PState[Expr, Contraction]) = ps.node.configuration match {
    case Let(_, _) => true
    case Ctr(_, _) => true
    case x => ps.node.ancestors.forall { a =>
      a.configuration match {
        case Let(_, _) => true
        case ae => !(HE.heByCoupling(ae, x) && HE.b(ae) == HE.b(x))
      }
    }
  }
}