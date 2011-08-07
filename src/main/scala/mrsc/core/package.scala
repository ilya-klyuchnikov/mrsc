package mrsc

package object core {
  type Path = List[Int]
  type CoPath = List[Int]

  /*! The following exception usually means that some modeling expectation (or hypothesis) 
 was not met during actual modeling.  */
  class ModelingError(val message: String) extends Exception(message: String)
}