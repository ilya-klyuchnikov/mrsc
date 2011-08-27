package mrsc.pfp

import mrsc.core._

case class Contraction[+C](v: Name, pat: C) {
  override def toString =
    if (v != null) v + " = " + pat else ""
  def subst() = Map[Name, C](v -> pat)
}

sealed trait DriveStep[+C]
case class TransientDriveStep[C](next: C) extends DriveStep[C]
case object StopDriveStep extends DriveStep[Nothing]
case class DecomposeDriveStep[C](compose: List[C] => C, parts: List[C]) extends DriveStep[C]
case class VariantsDriveStep[C](cases: List[(C, Contraction[C])]) extends DriveStep[C]

abstract sealed trait RuleStep[+C]
case object StopRuleStep extends RuleStep[Nothing]
case class VariantsRuleStep[C](cases: List[(Name, C)]) extends RuleStep[C]

abstract sealed class DriveInfo[+C]
case object TransientStepInfo extends DriveInfo[Nothing] {
  override def toString = "->"
}
case class DecomposeStepInfo[C](compose: List[C] => C) extends DriveInfo[C] {
  override def toString = ""
}
case class VariantsStepInfo[C](contr: Contraction[C]) extends DriveInfo[C] {
  override def toString = contr.toString
}

abstract class Extra[+C]
case object NoExtra extends Extra[Nothing]
case class RebuildingInfo[C](from: C) extends Extra[C]

trait PFPMachine[C] extends Machine[C, DriveInfo[C], Extra[C]] {
  type N = Node[C, DriveInfo[C], Extra[C]]
  type Warning
  def canFold(g: G): Option[N]
  def drive(g: G): List[S]
  def rebuildings(whistle: Option[Warning], g: G): List[S]
  def mayDiverge(g: G): Option[Warning]

  override def steps(g: G): List[S] =
    canFold(g) match {
      case Some(node) =>
        List(Fold(node))
      case _ =>
        val whistle = mayDiverge(g)
        val driveSteps =
          if (whistle.isEmpty) drive(g) else List(ToUnworkable(): S)
        val rebuildSteps = rebuildings(whistle, g)
        rebuildSteps ++ driveSteps 
    }
}

trait Driving[C] extends PFPMachine[C] with OperationalSemantics[C] {
  override def drive(g: G): List[S] =
    driveStep(g.current.conf) match {
      case StopDriveStep =>
        List(CompleteCurrentNode())
      case DecomposeDriveStep(compose, args) =>
        val stepInfo = DecomposeStepInfo(compose)
        val subSteps = args map { a => (a, stepInfo, NoExtra) }
        List(AddChildNodes(subSteps))
      case TransientDriveStep(next) =>
        val subSteps = List((next, TransientStepInfo, NoExtra))
        List(AddChildNodes(subSteps))
      case VariantsDriveStep(vs) =>
        val ns = vs map { v => (v._1, VariantsStepInfo(v._2), NoExtra) }
        List(AddChildNodes(ns))
    }
}

trait RenamingFolding[C] extends PFPMachine[C] with PFPSyntax[C] {
  override def canFold(g: G): Option[N] =
    g.current.ancestors.find { n => subclass.equiv(g.current.conf, n.conf) }
}

trait BinaryWhistle[C] extends PFPMachine[C] {
  type Warning = N
  val ordering: PartialOrdering[C]
  override def mayDiverge(g: G): Option[Warning] =
    g.current.ancestors find { n => ordering.lteq(n.conf, g.current.conf) }
}

trait UnaryWhistle[C] extends PFPMachine[C] {
  type Warning = Unit
  def dangerous(c: C): Boolean
  override def mayDiverge(g: G): Option[Warning] =
    if (dangerous(g.current.conf)) Some(Unit) else None
}

trait AllRebuildings[C] extends PFPMachine[C] with PFPSyntax[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] = {
    rebuildings(g.current.conf) map { Rebuild(_, NoExtra): S }
  }
}

trait LowerRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None    => List()
      case Some(_) => rebuildings(g.current.conf) map { Rebuild(_, NoExtra): S }
    }
}

trait UpperRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None        => List()
      case Some(upper) => rebuildings(upper.conf) map { Rollback(upper, _, NoExtra) }
    }
}

trait DoubleRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None =>
        List()
      case Some(upper) =>
        val rebuilds: List[S] =
          rebuildings(g.current.conf) map { Rebuild(_, NoExtra): S }
        val rollbacks: List[S] =
          rebuildings(upper.conf) map { Rollback(upper, _, NoExtra) }
        rollbacks ++ rebuilds 
    }
}

trait LowerAllBinaryGensOnBinaryWhistle[C] extends PFPMachine[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None        => List()
      case Some(upper) => mutualGens(g.current.conf, upper.conf) map translate map { Rebuild(_, NoExtra): S }
    }
}

trait UpperAllBinaryGensOnBinaryWhistle[C] extends PFPMachine[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None        => List()
      case Some(upper) => mutualGens(upper.conf, g.current.conf) map translate map { Rollback(upper, _, NoExtra) }
    }
}

trait DoubleAllBinaryGensOnBinaryWhistle[C] extends PFPMachine[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None =>
        List()
      case Some(upper) =>
        val rollbacks: List[S] = mutualGens(upper.conf, g.current.conf) map translate map { Rollback(upper, _, NoExtra) }
        val rebuilds: List[S] = mutualGens(g.current.conf, upper.conf) map translate map { Rebuild(_, NoExtra): S }
        rollbacks ++ rebuilds
    }
}

trait LowerAllBinaryGensOrDriveOnBinaryWhistle[C] extends PFPMachine[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None        => List()
      case Some(upper) => 
        val rebuilds: List[S] = mutualGens(g.current.conf, upper.conf) map translate map { Rebuild(_, NoExtra) : S }
        if (rebuilds.isEmpty) {
          drive(g)
        } else {
          rebuilds
        }
    }
}

trait UpperAllBinaryGensOrDriveOnBinaryWhistle[C] extends PFPMachine[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None        => List()
      case Some(upper) => 
          val rollbacks = mutualGens(upper.conf, g.current.conf) map translate map { Rollback(upper, _, NoExtra) }
          if (rollbacks.isEmpty) {
            drive(g)
          } else {
            rollbacks
          }
    }
}


trait UpperMsgOrLowerMggOnBinaryWhistle[C]
  extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[Warning], g: G): List[S] = {
    whistle match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(upperConf, currentConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val rollback = Rollback(upper, conf1, NoExtra)
            List(rollback)
          case None =>
            val cands = rawRebuildings(currentConf) filterNot trivialRb(currentConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => subclass.lteq(c2, c1) } }
            mgg.map(translate).map(c => Rebuild(c, NoExtra): S).toList
        }
      case None =>
        List()
    }
  }
}

// funny: most specific down or most general up
trait LowerMsgOrUpperMggOnBinaryWhistle[C] extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[Warning], g: G): List[S] = {
    whistle match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace = Rebuild(conf1, NoExtra): S
            List(replace)
          case None =>
            val cands = rawRebuildings(upperConf) filterNot trivialRb(upperConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => subclass.lteq(c2, c1) } }
            mgg.map(translate).map(Rollback(upper, _, NoExtra)).toList
        }
      case None =>
        List()
    }
  }
}

trait MSGCurrentOrDriving[C] extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[Warning], g: G): List[S] = {
    whistle match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace = Rebuild(conf1, NoExtra): S
            List(replace)
          case None =>
            drive(g)
        }
      case None =>
        List()
    }
  }
}

trait DoubleMsgOnBinaryWhistle[C] extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[Warning], g: G): List[S] = {
    whistle match {
      case Some(upper) =>
        val current = g.current
        val rollback = msg(upper.conf, current.conf) map { rb => Rollback(upper, translate(rb), NoExtra): S }
        val rebuild = msg(current.conf, upper.conf) map { rb => Rebuild(translate(rb), NoExtra): S }
        rollback.toList ++ rebuild.toList
      case None =>
        List()
    }
  }
}