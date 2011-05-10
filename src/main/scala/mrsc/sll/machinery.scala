package mrsc.sll

import mrsc._
import Signal._
import SLLGeneralizations._
import SLLExpressions._

// Here we define 2 variants of driving + 6 variants of rebuilding
// TODO Really this part can be generalized: it should not-be SLL-specific.

trait SLLSimpleDriving extends SLLDriving {
  def drive(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List(drive(pState))
      case _ =>
        List()
    }
}

trait SLLPruningDriving extends SLLDriving {
  def drive(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List(drive(pState))
      case _ =>
        List(MPrune)
    }
}

trait SLLCurentMsg extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamed = whistle.blamed.get
        val currentConf = pState.node.conf
        val blamedConf = blamed.conf
        val g = MSG.msg(currentConf, blamedConf)
        if (renaming(g.t, currentConf)) {
          val topSplitted = split(blamedConf)
          val rollback = MRollback(blamed, topSplitted, DummyExtra)
          List(rollback)
        } else if (g.t.isInstanceOf[Var]) {
          val let = split(currentConf)
          val replace = MReplace(let, DummyExtra)
          List(replace)
        } else {
          val let = Let(g.t, g.m1.toList)
          val replace = MReplace(let, DummyExtra)
          List(replace)
        }
    }
}

trait SLLBlamedMsg extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamed = whistle.blamed.get
        val currentConf = pState.node.conf
        val blamedConf = blamed.conf
        //println("msg")
        //println(blamedConf + "<" + currentConf)
        val g = MSG.msg(blamedConf, currentConf)
        if (g.t.isInstanceOf[Var] || renaming(g.t, blamedConf)) {
          val let = split(currentConf)
          val replace = MReplace(let, DummyExtra)
          //println("replace: " + let)
          List(replace)
        } else {
          val let = Let(g.t, g.m1.toList)
          val rollback = MRollback(blamed, let, DummyExtra)
          //println("rollback: " + let)
          List(rollback)
        }
    }
}

trait SLLAlwaysCurrentGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] = {
    val expr = pState.node.conf
    gens(expr) map { MReplace(_, DummyExtra) }
  }
}

trait SLLWhistleCurrentGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val expr = pState.node.conf
        gens(expr) map { MReplace(_, DummyExtra) }
    }
}

trait SLLWhistleBlamedGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamedNode = whistle.blamed.get
        val blamedExpr = blamedNode.conf
        val currentExpr = pState.node.conf
        gens(blamedExpr) map { MRollback(blamedNode, _, DummyExtra) }
    }
}

trait SLLWhistleAllrGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamedNode = whistle.blamed.get
        val blamedExpr = blamedNode.conf
        val currentExpr = pState.node.conf
        val blamed = SLLGeneralizations.gens(blamedExpr) map {
          MRollback(blamedNode, _, DummyExtra)
        }
        val current = SLLGeneralizations.gens(currentExpr) map {
          MReplace(_, DummyExtra)
        }
        blamed ++ current
    }
}

trait SLLNoTricks {
  def tricks(whistle: SLLSignal, pState: SLLState) =
    Nil
}
