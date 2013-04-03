import misc._

object PFP09MultiRuns extends scala.App {
  // showing all possible results
  multi.run("pfp/he1.pfp", multi.allDepthBound1(15))
  multi.run("pfp/he1.pfp", multi.allDepthBound1(20))

}
