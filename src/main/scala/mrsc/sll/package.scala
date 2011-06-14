package mrsc

import mrsc._

package object sll {
  type SLLState = PState[Expr, DriveInfo[Expr], Extra]
  type SLLStep = Command[Expr, DriveInfo[Expr], Extra]
  type SLLSignal = Option[CoNode[Expr, DriveInfo[Expr], Extra]]
}