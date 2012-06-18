package mrsc.pfp

import mrsc.core._

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

trait Driving extends PFPRules with PFPSemantics {
  override def drive(signal: Signal, g: G): List[S] =
    List(driveStep(g.current.conf).graphStep)
}

trait PositiveDriving extends PFPRules with PFPSyntax with PFPSemantics {
  override def drive(signal: Signal, g: G): List[S] = {
    val ds = driveStep(g.current.conf) match {
      case VariantsMStep(sel, bs) =>
        val bs1 = bs map { case (ptr, ctr, next) => (ptr, ctr, subst(next, Map(sel -> ctr))) }
        VariantsMStep(sel, bs1)
      case s =>
        s
    }
    List(ds.graphStep)
  }

}

trait Folding extends PFPRules with PFPSyntax {
  override def fold(signal: Signal, g: G): List[S] =
    g.current.ancestors.find { n => subclass.equiv(g.current.conf, n.conf) } map { n => FoldStep(n.sPath): S } toList
}

trait NoWhistle extends PFPRules {
  override def inspect(g: G): Signal = None
}

trait BinaryWhistle extends PFPRules {
  val ordering: PartialOrdering[MetaTerm]
  override def inspect(g: G): Signal =
    g.current.ancestors find { n => ordering.lteq(n.conf, g.current.conf) }
}

trait HEWhistle extends BinaryWhistle {
  override val ordering = HEOrdering
}

trait HEByCouplingWhistle extends BinaryWhistle {
  override val ordering = HEByCouplingOrdering
}

trait NoRebuildings extends PFPRules with PFPSyntax {
  override def rebuild(signal: Option[N], g: G) = List()
}

trait AllRebuildings extends PFPRules with PFPSyntax {
  override def rebuild(signal: Option[N], g: G) =
    rebuildings(g.current.conf) map { RebuildStep(_): S }
}

trait LowerRebuildingsOnBinaryWhistle extends PFPRules with PFPSyntax with BinaryWhistle {
  override def rebuild(signal: Signal, g: G) =
    signal match {
      case None    => List()
      case Some(_) => rebuildings(g.current.conf) map { RebuildStep(_): S }
    }
}

trait UpperRebuildingsOnBinaryWhistle extends PFPRules with PFPSyntax with BinaryWhistle {
  override def rebuild(signal: Signal, g: G) =
    signal match {
      case None        => List()
      case Some(upper) => rebuildings(upper.conf) map { RollbackStep(upper.sPath, _): S }
    }
}

trait DoubleRebuildingsOnBinaryWhistle extends PFPRules with PFPSyntax with BinaryWhistle {
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

trait LowerAllBinaryGensOnBinaryWhistle extends PFPRules with MutualGens with BinaryWhistle {
  override def rebuild(signal: Signal, g: G): List[S] =
    signal match {
      case None => List()
      case Some(upper) =>
        mutualGens(g.current.conf, upper.conf) map { RebuildStep(_): S }
    }
}

trait UpperAllBinaryGensOnBinaryWhistle extends PFPRules with MutualGens with BinaryWhistle {
  override def rebuild(signal: Signal, g: G): List[S] =
    signal match {
      case None => List()
      case Some(upper) =>
        mutualGens(upper.conf, g.current.conf) map { RollbackStep(upper.sPath, _): S }
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
        val rollbacks = mutualGens(upper.conf, g.current.conf) map { RollbackStep(upper.sPath, _): S }
        if (rollbacks.isEmpty) {
          drive(signal, g)
        } else {
          rollbacks
        }
    }
}

trait UpperMsgOrLowerMggOnBinaryWhistle extends PFPRules with MSG with BinaryWhistle {

  override def rebuild(signal: Signal, g: G): List[S] = {
    signal match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(upperConf, currentConf) match {
          case Some(rb) =>
            List(RollbackStep(upper.sPath, rb): S)
          case None =>
            val cands = rebuildings(currentConf) filterNot trivialRb(currentConf)
            val mgg = cands find { case Rebuilding(c1, _) => cands forall { case Rebuilding(c2, _) => subclass.lteq(c2, c1) } }
            mgg.map(RebuildStep(_): S).toList
        }
      case None =>
        List()
    }
  }
}

trait LowerMsgOrUpperMggOnBinaryWhistle extends PFPRules with MSG with BinaryWhistle {

  override def rebuild(signal: Signal, g: G): List[S] = {
    signal match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            List(RebuildStep(rb): S)
          case None =>
            val cands = rebuildings(upperConf) filterNot trivialRb(upperConf)
            val mgg = cands find { case Rebuilding(c1, _) => cands forall { case Rebuilding(c2, _) => subclass.lteq(c2, c1) } }
            mgg.map(RollbackStep(upper.sPath, _): S).toList
        }
      case None =>
        List()
    }
  }
}

trait LowerMsgOrDrivingOnBinaryWhistle extends PFPRules with MSG with BinaryWhistle {

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

trait DoubleMsgOnBinaryWhistle extends PFPRules with MSG with BinaryWhistle {

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
