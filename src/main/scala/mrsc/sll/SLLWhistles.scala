package mrsc.sll

import mrsc._

trait Whistle {
  def name: String
  def blame[D, E](ps: PState[Expr, D, E]): Option[CoNode[Expr, D, E]]
}

case class ExpressionSize(size: Int) extends Whistle {
  val name = "size < " + size
  def blame[D, E](ps: PState[Expr, D, E]) = ps.node.conf match {
    case Let(_, _) => None
    case Ctr(_, _) => None
    case x if x.size < size => Some(ps.node)
    case _ => None
  }
}

case class MaxGens(max: Int) extends Whistle {
  val name = "no more than " + max + " lets"
  def blame[D, E](ps: PState[Expr, D, E]): Option[CoNode[Expr, D, E]] = {
    val lets = (ps.node :: ps.node.ancestors).count(_.conf.isInstanceOf[Let])
    if (lets > max)
      Some(ps.node)
    else
      None
  }
}

case class MaxLetParts(max: Int) extends Whistle {
  val name = "no more than " + max + " let parts"
  def blame[D, E](ps: PState[Expr, D, E]): Option[CoNode[Expr, D, E]] = {
    val letOpt = (ps.node :: ps.node.ancestors).find(_.conf.isInstanceOf[Let])
    letOpt match {
      case Some(CoNode(Let(_, bs), _, _, _, _)) =>
        if (bs.length > max)
          Some(ps.node)
        else None
      case _ => None
    }

  }
}

object HEWhistle extends Whistle {
  val name = "homeomorphic embedding"
  def blame[D, E](ps: PState[Expr, D, E]) = ps.node.conf match {
    case Let(_, _) => None
    case Ctr(_, _) => None
    case x => ps.node.ancestors.find { a =>
      a.conf match {
        case Let(_, _) => false
        case ae => HE.he(ae, x)
      }
    }
  }
}

object HEWithRedexWhistle extends Whistle {
  val name = "homeomorphic embedding with redex"
  def blame[D, E](ps: PState[Expr, D, E]) = ps.node.conf match {
    case Let(_, _) => None
    case Ctr(_, _) => None
    case x => ps.node.ancestors.find { a =>
      a.conf match {
        case Let(_, _) => false
        case ae => HE.he_*(ae, x)
      }
    }
  }
}

object HEByCouplingWhistle extends Whistle {
  val name = "homeomorphic embedding via coupling"
  def blame[D, E](ps: PState[Expr, D, E]) = ps.node.conf match {
    case Let(_, _) => None
    case Ctr(_, _) => None
    case x => ps.node.ancestors.find { a =>
      a.conf match {
        case Let(_, _) => false
        case ae => HE.heByCoupling(ae, x)
      }
    }
  }
}

object HEByCouplingWithRedexWhistle extends Whistle {
  val name = "homeomorphic embedding via coupling with redex"
  def blame[D, E](ps: PState[Expr, D, E]) = ps.node.conf match {
    case Let(_, _) => None
    case Ctr(_, _) => None
    case x => ps.node.ancestors.find { a =>
      a.conf match {
        case Let(_, _) => false
        case ae => (HE.heByCoupling(ae, x) && HE.b(ae) == HE.b(x))
      }
    }
  }
}

object Whistles {
  def or(ws: Whistle*): Whistle =
    new Whistle {
      val name = ws map { _.name } mkString ("(", "||", ")")
      override def blame[D, E](ps: PState[Expr, D, E]): Option[CoNode[Expr, D, E]] = {
        for (w <- ws) {
          val r = w.blame(ps)
          if (r.isDefined) return r
        }
        None
      }
    }
}