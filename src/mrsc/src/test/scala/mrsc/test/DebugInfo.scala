package mrsc.test

import org.scalatest.FunSuite

trait DebugInfo extends FunSuite {
  val verbose = System.getProperty("mrsc.verbose", "false").toBoolean

  def debug(message: String) {
     if (verbose) info(message)
  }
}
