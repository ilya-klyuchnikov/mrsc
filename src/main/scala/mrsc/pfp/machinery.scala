package mrsc.pfp

import mrsc.core._

trait MultiResultSCRules[C, D] extends GraphRewriteRules[C, D] {

  type Warning
  def inspect(g: G): Option[Warning]

  def fold(g: G): Option[N]
  def drive(g: G): List[S]
  def rebuild(signal: Option[Warning], g: G): List[S]

  override def steps(g: G): List[S] = fold(g) match {
    case Some(node) =>
      List(FoldStep(node))
    case None =>
      val signal = inspect(g)
      val driveSteps = if (signal.isEmpty) drive(g) else List()
      val rebuildSteps = rebuild(signal, g)
      rebuildSteps ++ driveSteps
  }
}

trait Driving[C] extends PFPRules[C] with PFPSemantics[C] {
  override def drive(g: G): List[S] =
    List(driveStep(g.current.conf).graphStep)
}

trait Folding[C] extends PFPRules[C] with PFPSyntax[C] {
  override def fold(g: G): Option[N] =
    g.current.ancestors.find { n => subclass.equiv(g.current.conf, n.conf) }
}

trait BinaryWhistle[C] extends PFPRules[C] {
  type Warning = N
  val ordering: PartialOrdering[C]
  override def inspect(g: G): Option[Warning] =
    g.current.ancestors find
      { n => ordering.lteq(n.conf, g.current.conf) }
}

trait UnaryWhistle[C] extends PFPRules[C] {
  type Warning = Unit
  def dangerous(c: C): Boolean
  override def inspect(g: G): Option[Warning] =
    if (dangerous(g.current.conf)) Some(Unit) else None
}

trait AllRebuildings[C] extends PFPRules[C] with PFPSyntax[C] {
  override def rebuild(signal: Option[Warning], g: G) =
    rebuildings(g.current.conf) map { RebuildStep(_): S }
}

trait LowerRebuildingsOnBinaryWhistle[C] extends PFPRules[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuild(signal: Option[Warning], g: G) =
    signal match {
      case None =>
        List()
      case Some(_) =>
        rebuildings(g.current.conf) map { RebuildStep(_): S }
    }
}

trait UpperRebuildingsOnBinaryWhistle[C] extends PFPRules[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuild(signal: Option[Warning], g: G) =
    signal match {
      case None        => 
        List()
      case Some(upper) => 
        rebuildings(upper.conf) map { RollbackStep(upper, _) }
    }
}

trait DoubleRebuildingsOnBinaryWhistle[C] extends PFPRules[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuild(signal: Option[Warning], g: G) =
    signal match {
      case None =>
        List()
      case Some(upper) =>
        val rebuilds =
          rebuildings(g.current.conf) map { RebuildStep(_): S }
        val rollbacks =
          rebuildings(upper.conf) map { RollbackStep(upper, _) }
        rollbacks ++ rebuilds
    }
}

trait LowerAllBinaryGensOnBinaryWhistle[C] extends PFPRules[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuild(signal: Option[Warning], g: G): List[S] =
    signal match {
      case None => List()
      case Some(upper) =>
        mutualGens(g.current.conf, upper.conf) map translate map { RebuildStep(_): S }
    }
}

trait UpperAllBinaryGensOnBinaryWhistle[C] extends PFPRules[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuild(signal: Option[Warning], g: G): List[S] =
    signal match {
      case None => List()
      case Some(upper) =>
        mutualGens(upper.conf, g.current.conf) map translate map { RollbackStep(upper, _) }
    }
}

trait DoubleAllBinaryGensOnBinaryWhistle[C] extends PFPRules[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuild(signal: Option[Warning], g: G) = signal match {
    case None =>
      List()
    case Some(upper) =>
      val rollbacks = 
        mutualGens(upper.conf, g.current.conf) map 
          { c => RollbackStep(upper, translate(c)) }
      val rebuilds = 
        mutualGens(g.current.conf, upper.conf) map 
          { c => RebuildStep(translate(c)): S }
      rollbacks ++ rebuilds
  }
}

trait LowerAllBinaryGensOrDriveOnBinaryWhistle[C] extends PFPRules[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuild(signal: Option[Warning], g: G): List[S] =
    signal match {
      case None => List()
      case Some(upper) =>
        val rebuilds: List[S] = mutualGens(g.current.conf, upper.conf) map translate map { RebuildStep(_): S }
        if (rebuilds.isEmpty) {
          drive(g)
        } else {
          rebuilds
        }
    }
}

trait UpperAllBinaryGensOrDriveOnBinaryWhistle[C] extends PFPRules[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuild(signal: Option[Warning], g: G): List[S] =
    signal match {
      case None => List()
      case Some(upper) =>
        val rollbacks = mutualGens(upper.conf, g.current.conf) map translate map { RollbackStep(upper, _) }
        if (rollbacks.isEmpty) {
          drive(g)
        } else {
          rollbacks
        }
    }
}

trait UpperMsgOrLowerMggOnBinaryWhistle[C]
  extends PFPRules[C] with MSG[C] with BinaryWhistle[C] {

  override def rebuild(signal: Option[Warning], g: G): List[S] = {
    signal match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(upperConf, currentConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            List(RollbackStep(upper, conf1))
          case None =>
            val cands = rawRebuildings(currentConf) filterNot trivialRb(currentConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => subclass.lteq(c2, c1) } }
            mgg.map(translate).map(RebuildStep(_): S).toList
        }
      case None =>
        List()
    }
  }
}

trait LowerMsgOrUpperMggOnBinaryWhistle[C] extends PFPRules[C] with MSG[C] with BinaryWhistle[C] {

  override def rebuild(signal: Option[Warning], g: G): List[S] = {
    signal match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace = RebuildStep(conf1): S
            List(replace)
          case None =>
            val cands = rawRebuildings(upperConf) filterNot trivialRb(upperConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => subclass.lteq(c2, c1) } }
            mgg.map(translate).map(RollbackStep(upper, _)).toList
        }
      case None =>
        List()
    }
  }
}

trait LowerMsgOrDrivingOnBinaryWhistle[C] extends PFPRules[C] with MSG[C] with BinaryWhistle[C] {

  override def rebuild(signal: Option[Warning], g: G) = signal match {
    case Some(upper) =>
      val lowerConf = g.current.conf
      val upperConf = upper.conf
      msg(lowerConf, upperConf) match {
        case Some(rb) =>
          List(RebuildStep(translate(rb)))
        case None =>
          drive(g)
      }
    case None =>
      List()
  }
}

trait DoubleMsgOnBinaryWhistle[C] extends PFPRules[C] with MSG[C] with BinaryWhistle[C] {

  def rebuild(signal: Option[Warning], g: G) = signal match {
    case Some(upper) =>
      val current = g.current
      val rollbacks = msg(upper.conf, current.conf) map { rb => RollbackStep(upper, translate(rb)) }
      val rebuildings = msg(current.conf, upper.conf) map { rb => RebuildStep(translate(rb)): S }
      rollbacks.toList ++ rebuildings.toList
    case None =>
      List()
  }
}