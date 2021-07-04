package mrsc.pfp

import scalaz.Show

object NamelessShows {

  private def s(p: Ptr): String =
    p.name + p.args.map(_ => "_").mkString("(", ", ", ")")

  // "toString" for terms
  def s(t: MetaTerm): String = "*" * t.ticks + (t match {
    case BVar(i, _) =>
      i.toString
    case FVar(i, _) =>
      "<" + i + ">"
    case GVar(n, _) =>
      n
    case Abs(t, _) =>
      "(\\" + s(t) + ")"
    case App(t1, t2, _) =>
      "(" + s(t1) + " " + s(t2) + ")"
    case Let(v, in, _) =>
      "(let " + s(v) + " in " + s(in) + ")"
    case Fix(t, _) =>
      "(#" + s(t) + ")"
    case Ctr(n, args, _) =>
      n + args.map(s).mkString("(", ", ", ")")
    case Case(sel, bs, _) =>
      "case " + s(sel) + " of " + bs.map(b => s(b._1) + " -> " + s(b._2)).mkString("{", "; ", "}")
    case Rebuilding(t, sub, _) =>
      s(t) + "/" + sub.map(kv => s(kv._1) + " -> " + s(kv._2)).mkString("[", ", ", "]")

  })

  implicit def TermShow[T <: MetaTerm]: Show[T] = Show.shows(s)
}
