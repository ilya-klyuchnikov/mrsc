import mrsc.pfp.experiments._
import mrsc.pfp.test._

SimpleChecker.run("pfp/nat02.pfp", multi.allDepthBound1(10))

SimpleChecker.run("pfp/nat02.pfp", multi.allDepthBound1(20))

SimpleChecker.run("pfp/nat03.pfp", multi.allDepthBound1(20))