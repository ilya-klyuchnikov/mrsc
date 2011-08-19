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

sealed trait Extra[+C]
case object NoExtra extends Extra[Nothing]
case class RebuildingInfo[C](from: C) extends Extra[C]

trait PFPMachine[C] extends Machine[C, DriveInfo[C], Extra[C]] {
  type WS
  def canFold(g: G): Option[Path]
  def drive(g: G): List[G]
  def rebuildings(whistle: Option[WS], g: G): List[G]
  def inspect(g: G): Option[WS]

  override def steps(g :G): List[G] =
    canFold(g) match {
      case Some(path) =>
        List(g.fold(path))
      case _ =>
        val whistle = inspect(g)
        val driveSteps = 
          if (whistle.isEmpty) drive(g) else List(g.toUnworkable())
        val rebuildSteps = rebuildings(whistle, g)
        driveSteps ++ rebuildSteps
    }
}

trait Driving[C] extends PFPMachine[C] with OperationalSemantics[C] {
  override def drive(g: G): List[G] =
    driveStep(g.current.conf) match {
      case StopDriveStep =>
        List(g.completeLeaf())
      case DecomposeDriveStep(compose, args) =>
        val stepInfo = DecomposeStepInfo(compose)
        val subSteps = args map { a => (a, stepInfo, NoExtra) }
        List(g.addChildNodes(subSteps))
      case TransientDriveStep(next) =>
        val subSteps = List((next, TransientStepInfo, NoExtra))
        List(g.addChildNodes(subSteps))
      case VariantsDriveStep(vs) =>
        val ns = vs map { v => (v._1, VariantsStepInfo(v._2), NoExtra) }
        List(g.addChildNodes(ns))
    }
}

trait RenamingFolding[C] extends PFPMachine[C] with Syntax[C] {
  override def canFold(g: G): Option[Path] =
    g.current.ancestors.find { n => equiv(g.current.conf, n.conf) } map { _.path }
}

trait BinaryWhistle[C] extends PFPMachine[C] {
  type WS = Node[C, DriveInfo[C], Extra[C]]
  val ordering: PartialOrdering[C]
  override def inspect(g: G): Option[WS] =
    g.current.ancestors find { n => ordering.lteq(n.conf, g.current.conf) }
}

trait UnaryWhistle[C] extends PFPMachine[C] {
  type WS = Unit
  def dubious(c: C): Boolean
  override def inspect(g: G): Option[WS] =
    if (dubious(g.current.conf)) Some(Unit) else None
}

trait AllRebuildings[C] extends PFPMachine[C] with Syntax[C] {
  override def rebuildings(whistle: Option[WS], g: G): List[G] = {
    rebuildings(g.current.conf) map { g.rebuild(_, NoExtra) }
  }
}

trait LowerRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with Syntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[WS], g:G): List[G] =
    whistle match {
      case None    => List()
      case Some(_) => rebuildings(g.current.conf) map { g.rebuild(_, NoExtra) }
    }
}

trait UpperRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with Syntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[WS], g: G): List[G] =
    whistle match {
      case None        => List()
      case Some(upper) => rebuildings(upper.conf) map { g.rollback(upper, _, NoExtra) }
    }
}

trait DoubleRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with Syntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[WS], g:G): List[G] =
    whistle match {
      case None =>
        List()
      case Some(upper) =>
        val rebuilds =
          rebuildings(g.current.conf) map { g.rebuild(_, NoExtra) }
        val rollbacks =
          rebuildings(upper.conf) map { g.rollback(upper, _, NoExtra) }
        rebuilds ++ rollbacks
    }
}

trait UpperMsgOrLowerMggOnBinaryWhistle[C]
  extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[WS], g:G): List[G] = {
    whistle match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(upperConf, currentConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val rollback = g.rollback(upper, conf1, NoExtra)
            List(rollback)
          case None =>
            val cands = rawRebuildings(currentConf) filterNot trivialRb(currentConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => instanceOf(c2, c1) } }
            mgg.map(translate).map(g.rebuild(_, NoExtra)).toList
        }
      case None =>
        List()
    }
  }
}

// funny: most specific down or most general up
trait LowerMsgOrUpperMggOnBinaryWhistle[C] extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[WS], g:G): List[G] = {
    whistle match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace = g.rebuild(conf1, NoExtra)
            List(replace)
          case None =>
            val cands = rawRebuildings(upperConf) filterNot trivialRb(upperConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => instanceOf(c2, c1) } }
            mgg.map(translate).map(g.rollback(upper, _, NoExtra)).toList
        }
      case None =>
        List()
    }
  }
}

trait MSGCurrentOrDriving[C] extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[WS], g: G): List[G] = {
    whistle match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace = g.rebuild(conf1, NoExtra)
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

  def rebuildings(whistle: Option[WS], g: G): List[G] = {
    whistle match {
      case Some(upper) =>
        val current = g.current
        val replace = msg(current.conf, upper.conf) map { rb => g.rebuild(translate(rb), NoExtra) }
        val rollback = msg(upper.conf, current.conf) map { rb => g.rollback(upper, translate(rb), NoExtra) }
        rollback.toList ++ replace.toList
      case None =>
        List()
    }
  }
}