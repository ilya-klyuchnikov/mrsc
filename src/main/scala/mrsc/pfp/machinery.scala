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

trait PFPMultiMachine[C] extends Machine[C, DriveInfo[C], Extra[C]] {

  type W = Option[CoNode[C, DriveInfo[C], Extra[C]]]
  def isLeaf(pState: PS): Boolean
  def fold(pState: PS): Option[CoPath]
  def blame(pState: PS): W
  def drive(whistle: W, pState: PS): List[CMD]
  def rebuildings(whistle: W, pState: PS): List[CMD]

  override def steps(pState: PS): List[CMD] =
    if (isLeaf(pState))
      List(ConvertToLeaf)
    else fold(pState) match {
      case Some(path) =>
        List(Fold(path))
      case _ =>
        val signal = blame(pState)
        val driveSteps = drive(signal, pState)
        val rebuildSteps = rebuildings(signal, pState)
        driveSteps ++ rebuildSteps
    }
}

trait Driving[C] extends PFPMultiMachine[C] with OperationalSemantics[C] {
  override def drive(whistle: W, pState: PS): List[CMD] =
    drive(pState.current.conf) match {
      case StopDriveStep =>
        List()
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

  override def isLeaf(pState: PS) =
    !isDrivable(pState.current.conf)
}

trait SafeDriving[C] extends Driving[C] {
  override def drive(whistle: W, pState: PS): List[CMD] =
    whistle match {
      case Some(blamed) => List(Discard)
      case None         => super.drive(whistle, pState)
    }
}

trait RenamingFolding[C] extends PFPMultiMachine[C] with Syntax[C] {
  override def fold(pState: PS): Option[CoPath] =
    pState.current.ancestors.find { n => instance.equiv(pState.current.conf, n.conf) } map { _.coPath }
}

trait BinaryWhistle[C] extends PFPMultiMachine[C] {
  val ordering: PartialOrdering[C]
  override def blame(pState: PS): W =
    pState.current.ancestors find { n => ordering.lteq(n.conf, pState.current.conf) }
}

trait UnaryWhistle[C] extends PFPMultiMachine[C] {
  def unsafe(c: C): Boolean
  override def blame(pState: PS): W =
    if (unsafe(pState.current.conf)) Some(pState.current) else None
}

trait AlwaysCurrentGens[C] extends PFPMultiMachine[C] with Syntax[C] {
  override def rebuildings(whistle: W, pState: PS): List[CMD] = {
    rebuildings(pState.current.conf) map { Rebuild(_, NoExtra) }
  }
}

trait CurrentGensOnWhistle[C] extends PFPMultiMachine[C] with Syntax[C] {
  override def rebuildings(whistle: W, pState: PS): List[CMD] =
    whistle match {
      case None         => List()
      case Some(blamed) => rebuildings(pState.current.conf) map { Rebuild(_, NoExtra) }
    }
}

trait BlamedGensOnWhistle[C] extends PFPMultiMachine[C] with Syntax[C] {
  override def rebuildings(whistle: W, pState: PS): List[CMD] =
    whistle match {
      case None         => List()
      case Some(blamed) => rebuildings(blamed.conf) map { Rollback(blamed, _, NoExtra) }
    }
}

trait AllGensOnWhistle[C] extends PFPMultiMachine[C] with Syntax[C] {
  override def rebuildings(whistle: W, pState: PS): List[CMD] =
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val rebuilds =
          rebuildings(pState.current.conf) map { Rebuild(_, NoExtra) }
        val rollbacks =
          rebuildings(blamed.conf) map { Rollback(blamed, _, NoExtra) }
        rebuilds ++ rollbacks
    }
}

trait MSGBlamedOrSplitCurrent[C] extends PFPMultiMachine[C] with MSG[C] {

  def rebuildings(whistle: W, pState: PS): List[CMD] = {
    whistle match {
      case Some(blamed) =>
        val currentConf = pState.current.conf
        val blamedConf = blamed.conf
        msg(blamedConf, currentConf) match {
          // try MSG
          case Some(rb) =>
            val conf1 = translate(rb)
            val rollback = Rollback(blamed, conf1, NoExtra)
            List(rollback)
          // If there is no msg, then just split the down configuration
          case None =>
            // splitting the down configuration
            val cands = rawRebuildings(currentConf) filterNot trivialRb(currentConf)
            val cands1 = cands filter { case (c1, _) => cands forall { case (c2, _) => instance.lteq(c1, c2) } }
            val let = translate(cands1(0))
            //println("replace " + let)
            val replace = Rebuild(let, NoExtra)
            List(replace)
        }
      case None =>
        List()
    }
  }
}

// funny: most specific down or most general up
trait BinaryMSGDownOrUnaryMGGUp[C] extends PFPMultiMachine[C] with MSG[C] {

  def rebuildings(whistle: W, pState: PS): List[CMD] = {
    whistle match {
      case Some(blamed) =>
        val currentConf = pState.current.conf
        val blamedConf = blamed.conf
        msg(currentConf, blamedConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace = Rebuild(conf1, NoExtra)
            List(replace)
          case None =>
            val rbs = rawRebuildings(blamedConf) filterNot trivialRb(blamedConf)
            val cand = rbs find { case (c1, _) => rbs forall { case (c2, _) => instance.lteq(c1, c2) } } get
            val let = translate(cand)
            val rollback = Rollback(blamed, let, NoExtra)
            List(rollback)
        }
      case None =>
        List()
    }
  }
}

trait MSGCurrentOrDriving[C] extends PFPMultiMachine[C] with MSG[C] {

  def rebuildings(whistle: W, pState: PS): List[CMD] = {
    whistle match {
      case Some(blamed) =>
        val currentConf = pState.current.conf
        val blamedConf = blamed.conf
        msg(currentConf, blamedConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace = Rebuild(conf1, NoExtra)
            List(replace)
          case None =>
            drive(None, pState)
        }
      case None =>
        List()
    }
  }
}

trait MixMsg[C] extends PFPMultiMachine[C] with MSG[C] {

  def rebuildings(whistle: W, pState: PS): List[CMD] = {
    whistle match {
      case Some(blamed) =>
        val currentConf = pState.current.conf
        val blamedConf = blamed.conf
        val replace = msg(currentConf, blamedConf) map { msg1 =>
          Rebuild(translate(msg1), NoExtra)
        }
        val rollback = msg(blamedConf, currentConf) map { msg1 =>
          Rollback(blamed, translate(msg1), NoExtra)
        }
        rollback.toList ++ replace.toList
      case None =>
        List()
    }
  }
}