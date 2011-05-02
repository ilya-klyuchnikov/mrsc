package mrsc.sll

import mrsc._

trait Whistle {
  def name: String
  def blame[I](ps: PState[Expr, I]): Option[CoNode[Expr, I]]
}

case class ExpressionSize(size: Int) extends Whistle {
  val name = "size < " + size
  def blame[I](ps: PState[Expr, I]) = ps.node.configuration match {
    case Let(_, _) => None
    case Ctr(_, _) => None
    case x if x.size < size => Some(ps.node)
    case _ => None
  }
}

object HEWhistle extends Whistle {
  val name = "homeomorphic embedding"
  def blame[I](ps: PState[Expr, I]) = ps.node.configuration match {
    case Let(_, _) => None
    case Ctr(_, _) => None
    case x => ps.node.ancestors.find { a =>
      a.configuration match {
        case Let(_, _) => false
        case ae => HE.he(ae, x)
      }
    }
  }
}

object HEWithRedexWhistle extends Whistle {
  val name = "homeomorphic embedding with redex"
  def blame[I](ps: PState[Expr, I]) = ps.node.configuration match {
    case Let(_, _) => None
    case Ctr(_, _) => None
    case x => ps.node.ancestors.find { a =>
      a.configuration match {
        case Let(_, _) => false
        case ae => HE.he_*(ae, x)
      }
    }
  }
}

object HEByCouplingWhistle extends Whistle {
  val name = "homeomorphic embedding via coupling"
  def blame[I](ps: PState[Expr, I]) = ps.node.configuration match {
    case Let(_, _) => None
    case Ctr(_, _) => None
    case x => ps.node.ancestors.find { a =>
      a.configuration match {
        case Let(_, _) => false
        case ae => HE.heByCoupling(ae, x)
      }
    }
  }
}

object HEByCouplingWithRedexWhistle extends Whistle {
  val name = "homeomorphic embedding via coupling with redex"
  def blame[I](ps: PState[Expr, I]) = ps.node.configuration match {
    case Let(_, _) => None
    case Ctr(_, _) => None
    case x => ps.node.ancestors.find { a =>
      a.configuration match {
        case Let(_, _) => false
        case ae => (HE.heByCoupling(ae, x) && HE.b(ae) == HE.b(x))
      }
    }
  }
}