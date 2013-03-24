import mrsc.pfp.experiments._

// This example demonstrates/speculates the possibility of superlinear speed-up
// by introducing let expression (it superlinear in call-by-value setting)
// In some sense, supercompiler just detects sharing.
// f <1>;
// f = \x -> case x of {
//   Z() -> Q();
//  S(x1) -> P(f x1, f x1)
//}; is translated into
// (let f0 = (\x0 -> case x0 of {Z() -> Q(); S(_) -> (let f1 = (f0 x10) in P(f1, f1))}) in (f0 <1>))
object PFP06LetSuperlinear extends scala.App {
  multi.run("pfp/exp1.pfp", multi.allDepthBound2(9))
}
