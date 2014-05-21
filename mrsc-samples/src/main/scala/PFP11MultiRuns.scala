import misc._

object PFP11MultiRuns extends scala.App {
  // showing all possible results
  multi.run("pfp/nats/03.pfp", multi.allDepthBound1(20))
}
