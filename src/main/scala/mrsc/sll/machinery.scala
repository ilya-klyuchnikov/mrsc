package mrsc.sll

import mrsc._
import Whistle._
import SLLGeneralizations._

// Here we define 2 variants of driving + 6 variants of rebuilding

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
        val rebuiltConf = msg(pState.node.conf, whistle.blamed.get.conf)
        val rebuildStep = MReplace(rebuiltConf, DummyExtra)
        List(rebuildStep)
    }
}

trait SLLBlamedMsg extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamedNode = whistle.blamed.get
        val blamedExpr = blamedNode.conf
        val currentExpr = pState.node.conf
        val rebuiltConf = msg(blamedExpr, currentExpr)
        val rebuildStep = MRollback(blamedNode, rebuiltConf, DummyExtra)
        List(rebuildStep)
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
