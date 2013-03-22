package mrsc.pfp

// It is an open question how to get rid off this
// global state and to not over-complicate passing freevar.
trait VarGen {
  var freeVar: Int = 100
  def nextVar(x: Any = ()): FVar = {
    freeVar += 1
    FVar(freeVar)
  }
}

// Driving without positive information propagation
trait PFPSemantics extends VarGen {
  import NamelessSyntax._
  val gc: GContext
  def driveStep(t: MetaTerm): MStep = t match {
    case rb: Rebuilding =>
      DecomposeRebuildingMStep(rb)
    case t: Term => Decomposition.decompose(t) match {
      case ObservableVar(v) =>
        StopMStep
      case ObservableCon(c) =>
        DecomposeCtrMStep(c)
      case ObservableAbs(l) =>
        val fv = nextVar()
        val body1 = termSubstTop(fv, l.t)
        DecomposeAbsMStep(body1, fv)
      case ObservableVarApp(fv, args) =>
        DecomposeVarApp(fv, args)
      case context @ Context(RedexCall(f)) =>
        UnfoldMStep(context.replaceHole(gc(f.n)))
      case context @ Context(RedexFix(t1 @ Fix(body))) =>
        UnfoldMStep(context.replaceHole(termSubstTop(t1, body)))
      case context @ Context(RedexLamApp(Abs(t1), App(_, t2))) =>
        TransientMStep(context.replaceHole((termSubstTop(t2, t1))))
      case context @ Context(RedexCaseCtr(Ctr(name, args), Case(_, bs))) =>
        val Some((ptr, body)) = bs.find(_._1.name == name)
        val next = args.foldRight(body)(termSubstTop(_, _))
        TransientMStep(context.replaceHole(next))
      case context @ Context(RedexCaseAlt(v: FVar, Case(_, bs))) =>
        val xs = for { (ptr @ Ptr(name, args), body) <- bs } yield {
          val ctr = Ctr(name, args.map(nextVar))
          val next = ctr.args.foldRight(body)(termSubstTop(_, _))
          (ptr, ctr, context.replaceHole(next))
        }
        VariantsMStep(v, xs)
      case context @ Context(RedexCaseAlt(sel, Case(_, bs))) =>
        val v = nextVar()
        RebuildMStep(Rebuilding(context.replaceHole(Case(v, bs)), Map(v -> sel)))
      case context @ Context(RedexLet(Let(v, body))) =>
        val red1 = termSubstTop(v, body)
        TransientMStep(context.replaceHole(red1))
    }
  }
}

trait SimplePartialOrdering[T] extends PartialOrdering[T] {
  override def tryCompare(x: T, y: T): Option[Int] = (lteq(x, y), lteq(y, x)) match {
    case (false, false) =>
      None
    case (false, true) =>
      Some(1)
    case (true, false) =>
      Some(-1)
    case (true, true) =>
      Some(0)
  }
}
