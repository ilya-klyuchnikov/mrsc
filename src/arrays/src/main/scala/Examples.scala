package mrsc.pfp.arrays

import scalaz._
import Scalaz._
import mrsc.core._
import mrsc.pfp._

// IndexPA(Def(RepArray(l,v)), _) => v
// see notes
// Trick: parallel driving
object Ex01 extends scala.App with Demo {
  val bindings: GContext = io.bingingsFromFile("pfp/arrays/defs.pfp")
  // size > 0
  run("gt <1> Z()")
  // index < size
  run("lt <3> <1>")
  // the goal
  run("at (rep <1> <2>) <3>")
}

// case NestedArraySegments(Def(RepArray(l, vs))) =>
//  val lens - replicate(l, vs.length)
//  createSegmentsFromLens(lens)
object Ex02 extends scala.App with Demo {
  val bindings: GContext = io.bingingsFromFile("pfp/arrays/defs.pfp")
  // size > 0
  run("segLs (rep <1> <2>)")
  run("rep <1> (length <2>)")
}

// case FirstPA(Def(BackPermute(pairs, is))) => BackPermute(FirstPA(pairs), is)
// Trick: semantics with errors
object Ex03 extends scala.App with Demo {
  val bindings: GContext = io.bingingsFromFile("pfp/arrays/defs.pfp")
  run("firstPA (backPermute <1> <2>)")
  run("backPermute (firstPA <1>) <2>")
}

// case LengthPA(DefIndexPA(nested, i)) =>
//  val lens = segLens(nested)
//  lens(i)
object Ex04 extends scala.App with Demo {
  val bindings: GContext = io.bingingsFromFile("pfp/arrays/defs.pfp")
  run("length (at <1> <2>)")
  run("at (segLs <1>) <2>")
}

// case r@ReducePA(Def(rs@ReduceSeg(vs, _))) if rs.m == r.m => vs.reduce(r.m)
object Ex05 extends scala.App with Demo {
  val bindings: GContext = io.bingingsFromFile("pfp/arrays/defs.pfp")
  run("reduce (reduceNest <1> <2> <3>) <2> <3>")
  run("reduce (flatten <1>) <2> <3>")
}

// case r@ReduceNest(vs, xs ->> is) =>
object Ex06 extends scala.App with Demo {
  val bindings: GContext = io.bingingsFromFile("pfp/arrays/defs.pfp")
  run("reduce (reduceNest <1> <2> <3>) <2> <3>")
  run("reduce (flatten <1>) <2> <3>")
}

// case NestedArrayValues(Def(NRepArray(segs, Def(d@RepArray(l,v))))) => {   //see  (*)
//   val l1 = segs.bs.reduce
//   d.elem.replicate(l1,v)
// this example doesn't run with deforester
object Ex07 extends scala.App with Demo {
  val bindings: GContext = io.bingingsFromFile("pfp/arrays/defs.pfp")
  //run("take <1> <2>")
}




