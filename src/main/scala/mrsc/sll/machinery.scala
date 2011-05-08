package mrsc.sll

import mrsc._
import Whistle._ 

// classic single-result supercompiler
// current expression rebuilding
class SC1(val program: Program, val whistle: Whistle = HEWhistle)
  extends GenericMultiMachine[Expr, SubStepInfo, Extra, Blaming[Expr, SubStepInfo, Extra]]
  with SLLDriving
  with SLLFolding[SubStepInfo, Extra]
  with SLLWhistle
  with SLLRebuildings {

  override def drive(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]) =
    whistle.signal match {
      case OK =>
        drive(pState)
      case _ =>
        List()
  }

  override def rebuildings(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]) =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val rebuiltConf = msg(pState.node.conf, whistle.blamed.get.conf)
        val rebuildStep = MReplace(rebuiltConf, DummyExtra)
        List(rebuildStep)
    }

  override def tricks(whistle: Blaming[Expr, SubStepInfo, Extra], pState: PState[Expr, SubStepInfo, Extra]) =
    Nil
}