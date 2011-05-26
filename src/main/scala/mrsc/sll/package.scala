package mrsc

package object sll {
  type SLLState = PState[Expr, SubStepInfo[Expr], Extra]
  type SLLStep = Step[Expr, SubStepInfo[Expr], Extra]
  type SLLSignal = Blaming[Expr, SubStepInfo[Expr], Extra]
}