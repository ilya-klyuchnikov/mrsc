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
  def fold(pState: PS): Option[CoPath]
  def drive(pState: PS): List[CMD]
  def rebuildings(whistle: Option[WS], pState: PS): List[CMD]
  def inspect(pState: PS): Option[WS]

  override def steps(pState: PS): List[CMD] =
    fold(pState) match {
      case Some(path) =>
        List(Fold(path))
      case _ =>
        val signal = inspect(pState)
        val driveSteps = 
          if (signal.isEmpty) drive(pState) else List(Discard)
        val rebuildSteps = rebuildings(signal, pState)
        driveSteps ++ rebuildSteps
    }
}

trait Driving[C] extends PFPMachine[C] with OperationalSemantics[C] {
  override def drive(pState: PS): List[CMD] =
    driveStep(pState.current.conf) match {
      case StopDriveStep =>
        List(ConvertToLeaf)
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

trait RenamingFolding[C] extends PFPMachine[C] with Syntax[C] {
  override def fold(pState: PS): Option[CoPath] =
    pState.current.ancestors.find { n => instance.equiv(pState.current.conf, n.conf) } map { _.coPath }
}

trait BinaryWhistle[C] extends PFPMachine[C] {
  type WS = CoNode[C, DriveInfo[C], Extra[C]]
  val ordering: PartialOrdering[C]
  override def inspect(pState: PS): Option[WS] =
    pState.current.ancestors find { n => ordering.lteq(n.conf, pState.current.conf) }
}

case object UnarySignal
trait UnaryWhistle[C] extends PFPMachine[C] {
  type WS = UnarySignal.type
  def unsafe(c: C): Boolean
  override def inspect(pState: PS): Option[WS] =
    if (unsafe(pState.current.conf)) Some(UnarySignal) else None
}

trait AllRebuildings[C] extends PFPMachine[C] with Syntax[C] {
  override def rebuildings(whistle: Option[WS], pState: PS): List[CMD] = {
    rebuildings(pState.current.conf) map { Rebuild(_, NoExtra) }
  }
}

trait LowerRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with Syntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[WS], pState: PS): List[CMD] =
    whistle match {
      case None    => List()
      case Some(_) => rebuildings(pState.current.conf) map { Rebuild(_, NoExtra) }
    }
}

trait UpperRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with Syntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[WS], pState: PS): List[CMD] =
    whistle match {
      case None        => List()
      case Some(upper) => rebuildings(upper.conf) map { Rollback(upper, _, NoExtra) }
    }
}

trait DoubleRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with Syntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[WS], pState: PS): List[CMD] =
    whistle match {
      case None =>
        List()
      case Some(upper) =>
        val rebuilds =
          rebuildings(pState.current.conf) map { Rebuild(_, NoExtra) }
        val rollbacks =
          rebuildings(upper.conf) map { Rollback(upper, _, NoExtra) }
        rebuilds ++ rollbacks
    }
}

trait UpperMsgOrLowerMggOnBinaryWhistle[C] extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[WS], pState: PS): List[CMD] = {
    whistle match {
      case Some(upper) =>
        val currentConf = pState.current.conf
        val upperConf = upper.conf
        msg(upperConf, currentConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val rollback = Rollback(upper, conf1, NoExtra)
            List(rollback)
          case None =>
            val cands = rawRebuildings(currentConf) filterNot trivialRb(currentConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => instance.lteq(c1, c2) } }
            mgg.map(translate).map(Rebuild(_, NoExtra)).toList
        }
      case None =>
        List()
    }
  }
}

// funny: most specific down or most general up
trait LowerMsgOrUpperMggOnBinaryWhistle[C] extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[WS], pState: PS): List[CMD] = {
    whistle match {
      case Some(upper) =>
        val currentConf = pState.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace = Rebuild(conf1, NoExtra)
            List(replace)
          case None =>
            val cands = rawRebuildings(upperConf) filterNot trivialRb(upperConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => instance.lteq(c1, c2) } }
            mgg.map(translate).map(Rollback(upper, _, NoExtra)).toList
        }
      case None =>
        List()
    }
  }
}

trait MSGCurrentOrDriving[C] extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[WS], pState: PS): List[CMD] = {
    whistle match {
      case Some(upper) =>
        val currentConf = pState.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace = Rebuild(conf1, NoExtra)
            List(replace)
          case None =>
            drive(pState)
        }
      case None =>
        List()
    }
  }
}

trait DoubleMsgOnBinaryWhistle[C] extends PFPMachine[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[WS], pState: PS): List[CMD] = {
    whistle match {
      case Some(upper) =>
        val current = pState.current
        val replace = msg(current.conf, upper.conf) map { rb => Rebuild(translate(rb), NoExtra) }
        val rollback = msg(upper.conf, current.conf) map { rb => Rollback(upper, translate(rb), NoExtra) }
        rollback.toList ++ replace.toList
      case None =>
        List()
    }
  }
}