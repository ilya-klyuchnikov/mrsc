import mrsc.pfp.experiments._
import misc._

// demonstrates how it is possible to "smoke-test" correctness of
// transformations
object PFP07SimpleChecker extends scala.App {
  SimpleChecker.run("pfp/nats/02.pfp", multi.allDepthBound1(10))
  SimpleChecker.run("pfp/nats/02.pfp", multi.allDepthBound1(10))
  SimpleChecker.run("pfp/nats/03.pfp", multi.allDepthBound1(15))
}
