package mrsc.sll

import mrsc._
import Decomposition._
import SLLExpressions._

trait SLLWhistle {
  val whistle: Whistle

  def blame(pState: PState[Expr, DriveInfo[Expr], Extra]): SLLSignal =
    whistle.blame(pState)
}

trait SLLRebuildings {
  def msg(conf: Expr, wrt: Expr): Expr = {
    println("msg requested")
    println(conf)
    println(wrt)
    msgToLet(conf, MSG.msg(conf, wrt))
  }

  def gens(conf: Expr): List[Expr] =
    SLLGeneralizations.gens(conf)

  private def msgToLet(ce: Expr, g: Gen) =
    if (renaming(g.t, ce) || g.t.isInstanceOf[Var]) {
      split(ce)
    } else {
      Let(g.t, g.m1.toList)
    }

  protected def split(e: Expr): Expr =
    e match {
      case Ctr(name, args) => {
        val vs = args map freshVar
        Let(Ctr(name, vs), vs zip args)
      }
      case FCall(name, args) => {
        val vs = args map freshVar
        Let(FCall(name, vs), vs zip args)
      }
      case GCall(name, args) => {
        val vs = args map freshVar
        Let(GCall(name, vs), vs zip args)
      }
    }
}

