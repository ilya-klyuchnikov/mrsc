package mrsc.pfp.experiments

object examples extends App {
  // this example demonstrates the possibility of superlinear speed-up
  // in call-by-name setting:
  // (let f0 = (\x0 -> case x0 of {Z() -> Q(); S(_) -> (let f1 = (f0 x10) in P(f1, f1))}) in (f0 <1>))
  multi.run("pfp/exp1.pfp", multi.allDepthBound2(9))
}