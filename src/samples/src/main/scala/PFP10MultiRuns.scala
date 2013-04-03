import misc._

object PFP10MultiRuns extends scala.App {
  // showing all possible results
  multi.run("pfp/nats/02.pfp", multi.allDepthBound1(10))
  multi.run("pfp/nats/02.pfp", multi.allDepthBound1(20))

}
