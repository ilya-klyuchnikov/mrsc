package mrsc.pfp.samples

import mrsc.pfp._

object samples {
  def main(args: Array[String]) {
    MRSC.run("pfp/app1.pfp", AllMSC1)
    MRSC.run("pfp/app2.pfp", AllMSC1)
  }
}