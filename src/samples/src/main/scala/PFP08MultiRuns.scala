import misc._

object PFP08MultiRuns extends scala.App {
  // showing all possible results
  multi.run("pfp/app1.pfp", multi.allDepthBound1(10))
  multi.run("pfp/app1.pfp", multi.allDepthBound1(20))
  multi.run("pfp/app1.pfp", multi.allDepthBound1(30))
  multi.run("pfp/app1.pfp", multi.allDepthBound1(40))
}
