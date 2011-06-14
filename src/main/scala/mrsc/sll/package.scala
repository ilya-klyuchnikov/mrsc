package mrsc

package object sll {
  type SLLState = PState[Expr, DriveInfo[Expr], Extra]
  type SLLStep = Step[Expr, DriveInfo[Expr], Extra]
  type SLLSignal = Blaming[Expr, DriveInfo[Expr], Extra]
}