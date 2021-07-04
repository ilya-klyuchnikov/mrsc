package mrsc.core.test

import org.scalatest.FunSuite

/** Helper allowing to switch verbosity during tests.
  * To turn on verbose mode in sbt: set javaOptions += "-Dmrsc.verbose=true"
  */
trait DebugInfo extends FunSuite {
  val verbose: Boolean = System.getProperty("mrsc.verbose", "false").toBoolean

  def debug(message: String): Unit =
    if (verbose) info(message)
}
