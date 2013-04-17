package mrsc.pfp

import mrsc.core._

import NamelessSyntax._

trait PFPRules extends MRSCRules[MetaTerm, Label] {
  type Signal = Option[N]

  override def steps(g: G): List[S] = {
    val signal = inspect(g)
    fold(signal, g) match {
      case foldSteps if !foldSteps.isEmpty =>
        foldSteps
      case _ =>
        val driveSteps = if (signal.isEmpty) drive(signal, g) else List()
        val rebuildSteps = rebuild(signal, g)
        rebuildSteps ++ driveSteps
    }
  }
}

trait SizeGraphFilter extends PFPRules {
  val maxGraphSize: Int
  override def steps(g: G): List[S] =
    if (g.size > maxGraphSize)
      List()
    else
      super.steps(g)
}

trait Driving extends PFPRules with PFPSemantics {
  override def drive(signal: Signal, g: G): List[S] =
    List(driveStep(g.current.conf).graphStep)
}

trait LetDriving extends PFPRules with PFPSemantics {
  override def drive(signal: Signal, g: G): List[S] = {
    val mstep = driveStep(g.current.conf) match {
      case DecomposeRebuildingMStep(rb) =>
        FreezeRebuildingMStep(rb)
      case ms => ms
    }
    List(mstep.graphStep)
  }
}

trait PositiveDriving extends PFPRules with PFPSemantics {
  override def drive(signal: Signal, g: G): List[S] = {
    val ds = driveStep(g.current.conf) match {
      case VariantsMStep(sel, bs) =>
        val bs1 = bs map { case (ptr, ctr, next) => (ptr, ctr, applySubst(next, Map(sel -> ctr))) }
        VariantsMStep(sel, bs1)
      case s =>
        s
    }
    List(ds.graphStep)
  }
}

trait FoldingCandidates extends PFPRules {
  def foldingCandidates(n: N): List[N]
}

trait AllFoldingCandidates extends FoldingCandidates {
  override def foldingCandidates(n: N): List[N] = n.ancestors
}

trait Folding extends FoldingCandidates {
  override def fold(signal: Signal, g: G): List[S] =
    foldingCandidates(g.current) find { n => subclass.equiv(g.current.conf, n.conf) } map { n => FoldStep(n.sPath): S } toList
}

trait EmbeddingCandidates extends PFPRules {
  def embeddingCandidates(n: N): List[N]
}

trait AllEmbeddingCandidates extends EmbeddingCandidates {
  override def embeddingCandidates(n: N): List[N] = n.ancestors
}

trait ControlEmbeddingCandidates extends EmbeddingCandidates {

  override def embeddingCandidates(n: N): List[N] =
    if (isGlobal(n)) {
      n.ancestors.filter(isGlobal)
    } else {
      n.ancestors takeWhile { !isGlobal(_) } filter { !trivial(_) }
    }

  private def isGlobal(n: N): Boolean = n.conf match {
    case t: Rebuilding => false
    case t: Term =>
      Decomposition.decompose(t) match {
        case Context(RedexCaseAlt(_, _)) => true
        case _                           => false
      }
  }

  private def trivial(n: N): Boolean = n.conf match {
    case t: Rebuilding => true
    case t: Term =>
      Decomposition.decompose(t) match {
        case Context(RedexLamApp(lam, app)) => true
        case Context(_)                     => false
        // observable
        case _                              => true
      }
  }

}

trait NoWhistle extends PFPRules {
  override def inspect(g: G): Signal = None
}

trait BinaryWhistle extends EmbeddingCandidates {
  val ordering: PartialOrdering[MetaTerm]
  override def inspect(g: G): Signal =
    embeddingCandidates(g.current) find { n => ordering.lteq(n.conf, g.current.conf) }
}

trait HE3Whistle extends BinaryWhistle {
  override val ordering = HE3Ordering
}

trait HE3ByCouplingWhistle extends BinaryWhistle {
  override val ordering = HE3ByCouplingOrdering
}

// TODO: move into syntax
trait RebuildingsGenerator extends VarGen {

  import NamelessSyntax._

  def rebuildings(t: MetaTerm): List[Rebuilding] =
    t match {
      case t: Term => distinct(termRebuildings(t) filterNot trivialRb(t))
      case _       => List()
    }

  private def distinct(rbs: List[Rebuilding]): List[Rebuilding] = {
    var result: List[Rebuilding] = Nil
    val seen = scala.collection.mutable.HashSet[Rebuilding]()
    for (x <- rbs) {
      if (seen.find(r => subclass.equiv(r.t, x.t)).isEmpty) {
        result = x :: result
        seen += x
      }
    }
    result
  }

  private def trivialRb(c: MetaTerm)(rb: Rebuilding) =
    (rb.sub.values.toSet + rb.t) exists { subclass.equiv(c, _) }

  protected def termRebuildings(t: Term): List[Rebuilding] = rebuild(t, Map.empty)

  private def rebuild(e: Term, sub: Subst): List[Rebuilding] = {

    val rbs1: List[Rebuilding] = e match {
      case Abs(body, _) =>
        for { Rebuilding(t1, sub1, _) <- rebuild(body, sub) }
          yield Rebuilding(Abs(t1), sub1)
      case App(a1, a2, _) =>
        for {
          Rebuilding(t1, sub1, _) <- rebuild(a1, sub)
          Rebuilding(t2, sub2, _) <- rebuild(a2, sub1)
        } yield Rebuilding(App(t1, t2), sub2)
      case Let(a1, a2, _) =>
        for {
          Rebuilding(t1, sub1, _) <- rebuild(a1, sub)
          Rebuilding(t2, sub2, _) <- rebuild(a2, sub1)
        } yield Rebuilding(Let(t1, t2), sub2)
      case Fix(body, _) =>
        for { Rebuilding(t1, sub1, _) <- rebuild(body, sub) }
          yield Rebuilding(Fix(t1), sub1)
      case Ctr(n, xs, _) =>
        for { (ys, sub1) <- rebuild1(xs, sub) }
          yield Rebuilding(Ctr(n, ys), sub1)
      case Case(sel, bs, _) =>
        val (pts, bodies) = bs.unzip
        for {
          Rebuilding(sel1, sub1, _) <- rebuild(sel, sub)
          (bodies2, sub2) <- rebuild1(bodies, sub1)
        } yield Rebuilding(Case(sel1, pts zip bodies2), sub2)
      case _ =>
        List(Rebuilding(e, sub))
    }

    // extracting a term itself if it is extractable 
    val rbs2 =
      if (NamelessSyntax.isFreeSubTerm(e)) {
        val fn = nextVar()
        List(Rebuilding(fn, sub + (fn -> e)))
      } else
        List()

    // term is already extracted
    val rbs3 = for { (k, e1) <- sub if e1 == e } yield Rebuilding(k, sub)

    rbs1 ++ rbs2 ++ rbs3
  }

  // all combinations of rebuildings a list of expressions 
  private def rebuild1(es: List[Term], sub: Subst): List[(List[Term], Subst)] =
    (es :\ ((List[Term](), sub) :: Nil)) { (e, acc) =>
      for { (es1, sub) <- acc; Rebuilding(t, sub1, _) <- rebuild(e, sub) } yield (t :: es1, sub1)
    }
}

trait SizedRebuildingsGenerator extends RebuildingsGenerator {

  import NamelessSyntax._

  val genSize: Int

  override def rebuildings(t: MetaTerm): List[Rebuilding] =
    t match {
      case t: Term => distinct(termRebuildings(t) filterNot trivialRb(t))
      case _       => List()
    }

  private def distinct(rbs: List[Rebuilding]): List[Rebuilding] = {
    var result: List[Rebuilding] = Nil
    val seen = scala.collection.mutable.HashSet[Rebuilding]()
    for (x <- rbs) {
      if (seen.find(r => subclass.equiv(r.t, x.t)).isEmpty) {
        result = x :: result
        seen += x
      }
    }
    result
  }

  private def trivialRb(c: MetaTerm)(rb: Rebuilding) =
    (rb.sub.values.toSet + rb.t) exists { subclass.equiv(c, _) }

  override protected def termRebuildings(t: Term): List[Rebuilding] = rebuild(t, Map.empty)

  private def rebuild(e: Term, sub: Subst): List[Rebuilding] = {

    val rbs1: List[Rebuilding] = e match {
      case Abs(body, _) =>
        for { Rebuilding(t1, sub1, _) <- rebuild(body, sub) }
          yield Rebuilding(Abs(t1), sub1)
      case App(a1, a2, _) =>
        for {
          Rebuilding(t1, sub1, _) <- rebuild(a1, sub)
          Rebuilding(t2, sub2, _) <- rebuild(a2, sub1)
        } yield Rebuilding(App(t1, t2), sub2)
      case Let(a1, a2, _) =>
        for {
          Rebuilding(t1, sub1, _) <- rebuild(a1, sub)
          Rebuilding(t2, sub2, _) <- rebuild(a2, sub1)
        } yield Rebuilding(Let(t1, t2), sub2)
      case Fix(body, _) =>
        for { Rebuilding(t1, sub1, _) <- rebuild(body, sub) }
          yield Rebuilding(Fix(t1), sub1)
      case Ctr(n, xs, _) =>
        for { (ys, sub1) <- rebuild1(xs, sub) }
          yield Rebuilding(Ctr(n, ys), sub1)
      case Case(sel, bs, _) =>
        val (pts, bodies) = bs.unzip
        for {
          Rebuilding(sel1, sub1, _) <- rebuild(sel, sub)
          (bodies2, sub2) <- rebuild1(bodies, sub1)
        } yield Rebuilding(Case(sel1, pts zip bodies2), sub2)
      case _ =>
        List(Rebuilding(e, sub))
    }

    // extracting a term itself if it is extractable 

    val rbs2 =
      if (sub.size <= genSize && NamelessSyntax.isFreeSubTerm(e)) {
        val fn = nextVar()
        List(Rebuilding(fn, sub + (fn -> e)))
      } else
        List()

    // term is already extracted
    val rbs3 = for { (k, e1) <- sub if e1 == e } yield Rebuilding(k, sub)

    rbs1 ++ rbs2 ++ rbs3
  }

  // all combinations of rebuildings a list of expressions 
  private def rebuild1(es: List[Term], sub: Subst): List[(List[Term], Subst)] =
    (es :\ ((List[Term](), sub) :: Nil)) { (e, acc) =>
      for { (es1, sub) <- acc; Rebuilding(t, sub1, _) <- rebuild(e, sub) } yield (t :: es1, sub1)
    }
}
trait NoRebuildings extends PFPRules {
  override def rebuild(signal: Option[N], g: G) = List()
}

trait AllRebuildings extends PFPRules with RebuildingsGenerator {
  override def rebuild(signal: Option[N], g: G) = {
    val in = g.current.in
    in match {
      case SEdge(SNode(Rebuilding(_, _, _), _, _, _), _) =>
        List()
      case _ =>
        rebuildings(g.current.conf) map { x => RebuildStep(x): S }
    }
  }
}

trait LowerRebuildingsOnBinaryWhistle extends AllRebuildings with BinaryWhistle {
  override def rebuild(signal: Signal, g: G) =
    signal match {
      case None    => List()
      case Some(_) => super.rebuild(signal, g)
    }
}

trait LowerAllBinaryGensOnBinaryWhistle extends PFPRules with MutualGens with BinaryWhistle {
  override def rebuild(signal: Signal, g: G): List[S] =
    signal match {
      case None => List()
      case Some(upper) =>
        mutualGens(g.current.conf, upper.conf) map { RebuildStep(_): S }
    }
}

trait UpperRebuildingsOnBinaryWhistle extends PFPRules with RebuildingsGenerator with BinaryWhistle {
  override def rebuild(signal: Signal, g: G) =
    signal match {
      case None        => List()
      case Some(upper) => rebuildings(upper.conf) map { RollbackStep(upper.sPath, _): S }
    }
}

trait DoubleRebuildingsOnBinaryWhistle extends PFPRules with RebuildingsGenerator with BinaryWhistle {
  override def rebuild(signal: Option[N], g: G) =
    signal match {
      case None =>
        List()
      case Some(upper) =>
        val rebuilds =
          rebuildings(g.current.conf) map { RebuildStep(_): S }
        val rollbacks =
          rebuildings(upper.conf) map { RollbackStep(upper.sPath, _): S }
        rollbacks ++ rebuilds
    }
}

trait UpperAllBinaryGensOnBinaryWhistle extends PFPRules with MutualGens with BinaryWhistle {
  override def rebuild(signal: Signal, g: G): List[S] =
    signal match {
      case None => List()
      case Some(upper) =>
        val in = upper.in
        in match {
          case SEdge(SNode(Rebuilding(_, _, _), _, _, _), _) =>
            List()
          case _ =>
            mutualGens(upper.conf, g.current.conf) map { RollbackStep(upper.sPath, _): S }
        }
    }
}

trait DoubleAllBinaryGensOnBinaryWhistle extends PFPRules with MutualGens with BinaryWhistle {
  override def rebuild(signal: Signal, g: G) = signal match {
    case None =>
      List()
    case Some(upper) =>
      val rollbacks =
        mutualGens(upper.conf, g.current.conf) map { RollbackStep(upper.sPath, _): S }
      val rebuilds =
        mutualGens(g.current.conf, upper.conf) map { RebuildStep(_): S }
      rollbacks ++ rebuilds
  }
}

trait LowerAllBinaryGensOrDriveOnBinaryWhistle extends PFPRules with MutualGens with BinaryWhistle {
  override def rebuild(signal: Signal, g: G): List[S] =
    signal match {
      case None => List()
      case Some(upper) =>
        val rebuilds: List[S] = mutualGens(g.current.conf, upper.conf) map { RebuildStep(_): S }
        if (rebuilds.isEmpty) {
          drive(signal, g)
        } else {
          rebuilds
        }
    }
}

trait UpperAllBinaryGensOrDriveOnBinaryWhistle extends PFPRules with MutualGens with BinaryWhistle {
  override def rebuild(signal: Signal, g: G): List[S] =
    signal match {
      case None => List()
      case Some(upper) =>
        val in = upper.in
        in match {
          case SEdge(SNode(Rebuilding(_, _, _), _, _, _), _) =>
            List()
          case _ =>
            val rollbacks = mutualGens(upper.conf, g.current.conf) map { RollbackStep(upper.sPath, _): S }
            if (rollbacks.isEmpty) {
              drive(signal, g)
            } else {
              rollbacks
            }
        }
    }
}

trait UpperMsgOnBinaryWhistle extends PFPRules with MSGRebuildings with BinaryWhistle {

  override def rebuild(signal: Signal, g: G): List[S] = {
    signal match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(upperConf, currentConf) match {
          case Some(rb) =>
            List(RollbackStep(upper.sPath, rb): S)
          case None =>
            throw new Exception("Cannot msg " + upperConf + " and " + currentConf)
        }
      case None =>
        List()
    }
  }
}

trait UpperMsgOrLowerMggOnBinaryWhistle extends PFPRules with MSGRebuildings with BinaryWhistle {
  override def rebuild(signal: Signal, g: G): List[S] = {
    signal match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(upperConf, currentConf) match {
          case Some(rb) =>
            List(RollbackStep(upper.sPath, rb): S)
          case None =>
            val cands = rebuildings(currentConf)
            val mgg = cands find { case Rebuilding(c1, _, _) => cands forall { case Rebuilding(c2, _, _) => subclass.lteq(c2, c1) } }
            mgg.map(RebuildStep(_): S).toList
        }
      case None =>
        List()
    }
  }
}

trait LowerMsgOrUpperMggOnBinaryWhistle extends PFPRules with MSGRebuildings with BinaryWhistle {
  import NamelessSyntax._
  override def rebuild(signal: Signal, g: G): List[S] = {
    signal match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            List(RebuildStep(rb): S)
          case None =>
            val cands = rebuildings(upperConf)
            val mgg = cands find { case Rebuilding(c1, _, _) => cands forall { case Rebuilding(c2, _, _) => subclass.lteq(c2, c1) } }
            mgg.map(RollbackStep(upper.sPath, _): S).toList
        }
      case None =>
        List()
    }
  }
}

trait LowerMsgOrDrivingOnBinaryWhistle extends PFPRules with MSGRebuildings with BinaryWhistle {

  override def rebuild(signal: Signal, g: G) = signal match {
    case Some(upper) =>
      val lowerConf = g.current.conf
      val upperConf = upper.conf
      msg(lowerConf, upperConf) match {
        case Some(rb) => List(RebuildStep(rb))
        case None     => drive(signal, g)
      }
    case None =>
      List()
  }
}

trait LowerMsgOrUpperMsgOnBinaryWhistle extends PFPRules with MSGRebuildings with BinaryWhistle {

  override def rebuild(signal: Signal, g: G) = signal match {
    case Some(upper) =>
      val lowerConf = g.current.conf
      val upperConf = upper.conf
      msg(lowerConf, upperConf) match {
        case Some(rb) =>
          List(RebuildStep(rb))
        case None =>
          msg(upperConf, lowerConf) match {
            case Some(rb) =>
              List(RollbackStep(upper.sPath, rb): S)
            case None =>
              throw new Exception("Cannot msg " + upperConf + " and " + lowerConf)
          }
      }
    case None =>
      List()
  }
}

trait UpperMsgOrLowerMsgOnBinaryWhistle extends PFPRules with MSGRebuildings with BinaryWhistle {

  override def rebuild(signal: Signal, g: G) = signal match {
    case Some(upper) =>
      val lowerConf = g.current.conf
      val upperConf = upper.conf
      msg(upperConf, lowerConf) match {
        case Some(rb) =>
          List(RollbackStep(upper.sPath, rb))
        case None =>
          msg(lowerConf, upperConf) match {
            case Some(rb) =>
              List(RebuildStep(rb))
            case None =>
              throw new Exception("Cannot msg " + upperConf + " and " + lowerConf)
          }
      }
    case None =>
      List()
  }
}

trait DoubleMsgOnBinaryWhistle extends PFPRules with MSGRebuildings with BinaryWhistle {

  def rebuild(signal: Signal, g: G) = signal match {
    case Some(upper) =>
      val current = g.current
      val rollbacks = msg(upper.conf, current.conf) map { RollbackStep(upper.sPath, _): S }
      val rebuildings = msg(current.conf, upper.conf) map { RebuildStep(_): S }
      rollbacks.toList ++ rebuildings.toList
    case None =>
      List()
  }
}

trait MutualGens extends RebuildingsGenerator {
  def mutualGens(c1: MetaTerm, c2: MetaTerm): List[Rebuilding] = {
    val nonTrivialRbs = rebuildings(c1)
    nonTrivialRbs filter { rb => subclass.gteq(rb.t, c2) }
  }
}

trait MSGRebuildings extends MSG with RebuildingsGenerator {
  import NamelessSyntax._
  def msg(c1: MetaTerm, c2: MetaTerm): Option[Rebuilding] = (c1, c2) match {
    case (t1: Term, t2: Term) => strictTermMSG(t1, t2)
    case _                    => None
  }
}

trait FoldingOnBinaryWhistle extends PFPRules with BinaryWhistle {
  override def fold(signal: Signal, g: G): List[S] =
    signal.map(n => FoldStep(n.sPath): S).toList

  override def rebuild(signal: Signal, g: G): List[S] =
    Nil
}

