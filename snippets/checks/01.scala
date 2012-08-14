import mrsc.pfp.experiments._
import mrsc.pfp.test._

SimpleChecker.run("pfp/nats/02.pfp", multi.allDepthBound1(10))

SimpleChecker.run("pfp/nats/02.pfp", multi.allDepthBound1(20))

SimpleChecker.run("pfp/nats/03.pfp", multi.allDepthBound1(20))