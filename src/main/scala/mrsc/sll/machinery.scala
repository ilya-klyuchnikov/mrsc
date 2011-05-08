package mrsc.sll

import mrsc._
import Whistle._

// Here we define 2 variants of driving + 6 variants of rebuilding

trait SLLSimpleDriving extends SLLDriving {
  def drive(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]): List[MStep[Expr, SubStepInfo, Extra]] =
    whistle.signal match {
      case OK =>
        List(drive(pState))
      case _ =>
        List()
    }
}

trait SLLPruningDriving extends SLLDriving {
  def drive(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]): List[MStep[Expr, SubStepInfo, Extra]] =
    whistle.signal match {
      case OK =>
        List(drive(pState))
      case _ =>
        List(MPrune)
    }
}

trait SLLCurentMsg extends SLLRebuildings {
  def rebuildings(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]): List[MStep[Expr, SubStepInfo, Extra]] =
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
  def rebuildings(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]): List[MStep[Expr, SubStepInfo, Extra]] =
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
  def rebuildings(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]): List[MStep[Expr, SubStepInfo, Extra]] = {
    val expr = pState.node.conf
    SLLGeneralizations.gens(expr) map { MReplace(_, DummyExtra) }
  }
}

trait SLLWhistleCurrentGens extends SLLRebuildings {
  def rebuildings(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]): List[MStep[Expr, SubStepInfo, Extra]] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val expr = pState.node.conf
        SLLGeneralizations.gens(expr) map { MReplace(_, DummyExtra) }
    }
}

trait SLLWhistleBlamedGens extends SLLRebuildings {
  def rebuildings(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]): List[MStep[Expr, SubStepInfo, Extra]] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamedNode = whistle.blamed.get
        val blamedExpr = blamedNode.conf
        val currentExpr = pState.node.conf
        SLLGeneralizations.gens(blamedExpr) map { MRollback(blamedNode, _, DummyExtra) }
    }
}

trait SLLWhistleAllrGens extends SLLRebuildings {
  def rebuildings(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]): List[MStep[Expr, SubStepInfo, Extra]] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamedNode = whistle.blamed.get
        val blamedExpr = blamedNode.conf
        val currentExpr = pState.node.conf
        val blamed = SLLGeneralizations.gens(blamedExpr) map { MRollback(blamedNode, _, DummyExtra) }
        val current = SLLGeneralizations.gens(currentExpr) map { MReplace(_, DummyExtra) }
        blamed ++ current
    }
}


trait SLLNoTricks {
  def tricks(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]) =
    Nil
}

// classic single-result supercompiler
// current expression rebuilding
class SC1(val program: Program, val whistle: Whistle = HEWhistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, Blaming[Expr, SubStepInfo, Extra]]
  with SLLSimpleDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLCurentMsg
  with SLLNoTricks

class SC2(val program: Program, val whistle: Whistle = HEByCouplingWhistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, Blaming[Expr, SubStepInfo, Extra]]
  with SLLSimpleDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLBlamedMsg
  with SLLNoTricks

class SC3(val program: Program, val whistle: Whistle = HEByCouplingWhistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, Blaming[Expr, SubStepInfo, Extra]]
  with SLLSimpleDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLBlamedMsg
  with SLLNoTricks
