package mrsc

package object sll {
  type SLLState = PState[Expr, SubStepInfo, Extra]
  type SLLStep = MStep[Expr, SubStepInfo, Extra]
  type SLLSignal = Blaming[Expr, SubStepInfo, Extra]
}