package mrsc.core.test

import org.scalatest.funsuite.AnyFunSuite

/** Helper allowing to switch verbosity during tests.
  * To turn on verbose mode in sbt: set javaOptions += "-Dmrsc.verbose=true"
  */
trait DebugInfo extends AnyFunSuite {
  val verbose: Boolean = System.getProperty("mrsc.verbose", "false").toBoolean

  def debug(message: String): Unit =
    if (verbose) info(message)
}
