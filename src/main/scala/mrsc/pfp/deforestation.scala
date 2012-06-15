package mrsc.pfp

import mrsc.core._
import Syntax._

case class Deforester(gc: GContext) extends GraphRewriteRules[Term, Label] with VarGen {

  override def steps(g: G): List[S] = List(fold(g) getOrElse drive(g))

  def drive(g: G): S =
    AddChildNodesStep(driveStep(g.current.conf))

  def driveStep(t: Term): List[(Term, Label)] = t match {
    case GVar(n) =>
      List((gc(n), TransientLabel))
    case FVar(n) =>
      List()
    case Ctr(n, args) =>
      args.map(a => (a, CtrArgLabel))
    case App(Abs(t1), t2) =>
      List((termSubstTop(t2, t1), TransientLabel))
    case App(t1, t2) =>
      for ((n, s) <- driveStep(t1))
        yield (App(n, t2), s)
    case Fix(Abs(body)) =>
      List((termSubstTop(t, body), TransientLabel))
    case Fix(Ctr(_, _)) =>
      sys.error("unexpected term: " + t)
    case Fix(t1) =>
      for ((n, s) <- driveStep(t1))
        yield (Fix(n), s)
    case Case(Ctr(name, args), bs) =>
      val Some((ptr, body)) = bs.find(_._1.name == name)
      val next = args.foldRight(body)(termSubstTop(_, _))
      List((next, TransientLabel))
    case Case(t @ FVar(_), bs) =>
      for { (ptr@Ptr(name, args), body) <- bs } yield {
        val ctr = Ctr(name, args.map(nextVar))
        val next = ctr.args.foldRight(body)(termSubstTop(_, _))
        (next, CaseBranchLabel(t, ptr, ctr))
      }
    case Case(t1, bs) =>
      for ((n, s) <- driveStep(t1))
        yield (Case(n, bs), s)
    case Let(v @ Fix(_), body) =>
      List((termSubstTop(v, body), TransientLabel))
    case _ =>
      sys.error("unexpected term: " + t)
  }

  def fold(g: G): Option[S] = {
    val conf = g.current.conf
    val base = g.current.ancestors.find(n => Syntax.renaming(n.conf, conf))
    base.map(n => FoldStep(n.sPath))
  }
}
