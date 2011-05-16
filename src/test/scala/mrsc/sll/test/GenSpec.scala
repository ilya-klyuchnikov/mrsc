package mrsc.sll.test

import mrsc.sll._

import org.scalacheck.{ Gen => G, _ }
import G._
import org.scalacheck.Prop._

import SLLGen._
import SLLExpressions._

object GenSpec extends Properties("Generalizaions") {
  property("msg") = Prop.forAll { (e1: Expr, e2: Expr) =>
    val g = MSG.msg(e1, e2)
    ("evidence = " + g) |: all(
      "t/sub1 = e1" |: (subst(g.t, g.m1) == e1),
      "t/sub2 = e2" |: (subst(g.t, g.m2) == e2))
  }

  property("generalizations") = Prop.forAll { (e: Expr) =>
    println(e)
    val gens = SLLGeneralizations.gens(e)
    //println(gens)
    //println()

    val props = gens.map { g =>
      val Let(t, bindings) = g
      val sub = Map(bindings: _*)
      ("gen = " + g) |: (subst(t, sub) == e)
    }
    
    all(props: _*)
  }
}