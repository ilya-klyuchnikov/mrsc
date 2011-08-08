package mrsc.pfp.sll

object PrettySLL extends org.kiama.util.PrettyPrinter {
  def pretty(t: Expr) = super.pretty(show(t), 12)

  private def show(e: Expr): Doc = e match {
    case Where(e, defs) =>
      text("let {") <>
        nest(defs.foldLeft(empty) { (d, deff) =>
          d <>
            line <>
            deff.lhs.toDoc <>
            text(" = ") <>
            nest(line <> show(deff.rhs) <> text(";"), 2)
        }, 2) <>
        line <>
        text("} in ") <> show(e)

    case _ => text(e.toString)
  }

}