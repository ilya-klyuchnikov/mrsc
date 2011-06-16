package mrsc.sll

import mrsc._

/*
case class ExpressionSize(size: Int) extends PartialOrdering[Expr] {
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
*/

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

/*
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
}*/