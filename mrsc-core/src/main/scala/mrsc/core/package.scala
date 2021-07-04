package mrsc

package object core {
  type TPath = List[Int]
  type SPath = List[Int]

  /*! The following exception usually means that some modeling expectation (or hypothesis)
 was not met during actual modeling.  */
  class ModelingError(val message: String) extends Exception(message: String)
}
