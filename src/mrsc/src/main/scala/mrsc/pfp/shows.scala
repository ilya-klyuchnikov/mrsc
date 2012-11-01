package mrsc.pfp

import scalaz._
import Scalaz._

object NamelessShows {

  private def s(p: Ptr): String =
    p.name + p.args.map(_ => "_").mkString("(", ", ", ")")

  // "toString" for terms
  private def s(t: MetaTerm): String = t match {
    case BVar(i) =>
      i.toString
    case FVar(i) =>
      "<" + i + ">"
    case GVar(n) =>
      n
    case Abs(t)  =>
      "(\\" + s(t) + ")"
    case App(t1, t2) =>
      "(" + s(t1) + " " + s(t2) + ")"
    case Let(v, in) =>
      "(let " + s(v) + " in " + s(in) + ")"
    case Fix(t) =>
      "(#" + s(t) + ")"
    case Ctr(n, args) =>
      n + args.map(s).mkString("(", ", ", ")")
    case Case(sel, bs) =>
      "case " + s(sel) + " of " + bs.map(b => s(b._1) + " -> " + s(b._2)).mkString("{", "; ", "}")
    case Rebuilding(t, sub) => 
      s(t) + "/" + sub.map( kv => s(kv._1) + " -> " + s(kv._2)).mkString("[", ", ", "]")
      
  }

  implicit def TermShow[T <: MetaTerm]: Show[T] = shows(s)
}
