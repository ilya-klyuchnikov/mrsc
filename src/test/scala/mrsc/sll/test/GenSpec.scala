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
    val gens = SLLRebuilding.gens(e)
    println(gens.length + " generalizations")
    println()
    val props1 = gens.map { g =>
      val Let(t, bindings) = g
      val sub = Map(bindings map {case (k, v) => (Var(k), v)}: _*)
      ("t/sub == e, gen = " + g) |: (subst(t, sub) == e)
    }

    /*
    val props2 = gens.map { g =>
      val Let(t, bindings) = g
      ("let should be unique: " + g) |: (gens.filter(l => renaming(l.term, t)).length == 1)
    }*/

    all(props1: _*) //&& all(props2: _*)
  }
}