package mrsc.pfp

import mrsc.core._

sealed abstract class Extra[+C]
case object NoExtra extends Extra[Nothing]
final case class RebuildingInfo[C](from: C) extends Extra[C]

case class Contraction[+C](v: Name, pat: C) {
  override def toString =
    if (v != null) v + " = " + pat else ""
  def subst() = Map[Name, C](v -> pat)
}

abstract sealed class DriveInfo[+C]
case object TransientStepInfo extends DriveInfo[Nothing] {
  override def toString = "->"
}
final case class DecomposeStepInfo[C](compose: List[C] => C) extends DriveInfo[C] {
  override def toString = ""
}
final case class VariantsStepInfo[C](contr: Contraction[C]) extends DriveInfo[C] {
  override def toString = contr.toString
}

trait DriveSteps[C]
  extends MachineSteps[C, DriveInfo[C]] {

  def transientDriveStep(next: C): S = {
    val subSteps = List((next, TransientStepInfo)): List[(C, DriveInfo[C])]
    addChildNodes(subSteps)
  }

  def stopDriveStep: S =
    completeCurrentNode

  def decomposeDriveStep(compose: List[C] => C, parts: List[C]): S = {
    val stepInfo = DecomposeStepInfo(compose)
    val subSteps = parts map { a => (a, stepInfo) }
    addChildNodes(subSteps)
  }

  def variantsDriveStep(cases: List[(C, Contraction[C])]): S = {
    val ns = cases map { v => (v._1, VariantsStepInfo(v._2)) }
    addChildNodes(ns)

  }
}

trait PFPMachine[C] extends Machine[C, DriveInfo[C]]
  with MachineSteps[C, DriveInfo[C]] {

  type N = Node[C, DriveInfo[C]]
  type Warning
  def canFold(g: G): Option[N]
  def drive(g: G): List[S]
  def rebuildings(whistle: Option[Warning], g: G): List[S]
  def mayDiverge(g: G): Option[Warning]

  override def steps(g: G): List[S] =
    canFold(g) match {
      case Some(node) =>
        List(fold(node))
      case _ =>
        val whistle = mayDiverge(g)
        val driveSteps = if (whistle.isEmpty) drive(g) else List()
        val rebuildSteps = rebuildings(whistle, g)
        rebuildSteps ++ driveSteps
    }
}

trait PFPDriving[C] extends PFPMachine[C]
  with StepSignature[C, DriveInfo[C]] {
  def driveConf(c: C): S
  override def drive(g: G): List[S] = List(driveConf(g.current.conf))
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
    rebuildings(g.current.conf) map rebuild
  }
}

trait LowerRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None    => List()
      case Some(_) => rebuildings(g.current.conf) map rebuild
    }
}

trait UpperRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None        => List()
      case Some(upper) => rebuildings(upper.conf) map { rollback(upper, _) }
    }
}

trait DoubleRebuildingsOnBinaryWhistle[C] extends PFPMachine[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None =>
        List()
      case Some(upper) =>
        val rebuilds: List[S] =
          rebuildings(g.current.conf) map rebuild
        val rollbacks: List[S] =
          rebuildings(upper.conf) map { rollback(upper, _) }
        rollbacks ++ rebuilds
    }
}

trait LowerAllBinaryGensOnBinaryWhistle[C] extends PFPMachine[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None => List()
      case Some(upper) =>
        mutualGens(g.current.conf, upper.conf) map translate map rebuild
    }
}

trait UpperAllBinaryGensOnBinaryWhistle[C] extends PFPMachine[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None => List()
      case Some(upper) =>
        mutualGens(upper.conf, g.current.conf) map translate map { rollback(upper, _) }
    }
}

trait DoubleAllBinaryGensOnBinaryWhistle[C] extends PFPMachine[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None =>
        List()
      case Some(upper) =>
        val rollbacks: List[S] = mutualGens(upper.conf, g.current.conf) map translate map { rollback(upper, _) }
        val rebuilds: List[S] = mutualGens(g.current.conf, upper.conf) map translate map rebuild
        rollbacks ++ rebuilds
    }
}

trait LowerAllBinaryGensOrDriveOnBinaryWhistle[C] extends PFPMachine[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None => List()
      case Some(upper) =>
        val rebuilds: List[S] = mutualGens(g.current.conf, upper.conf) map translate map rebuild
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
      case None => List()
      case Some(upper) =>
        val rollbacks = mutualGens(upper.conf, g.current.conf) map translate map { rollback(upper, _) }
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
            List(rollback(upper, conf1))
          case None =>
            val cands = rawRebuildings(currentConf) filterNot trivialRb(currentConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => subclass.lteq(c2, c1) } }
            mgg.map(translate).map(rebuild).toList
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
            val replace = rebuild(conf1)
            List(replace)
          case None =>
            val cands = rawRebuildings(upperConf) filterNot trivialRb(upperConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => subclass.lteq(c2, c1) } }
            mgg.map(translate).map(rollback(upper, _)).toList
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
            val replace = rebuild(conf1)
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
        val rollbacks = msg(upper.conf, current.conf) map { rb => rollback(upper, translate(rb)) }
        val rebuildings = msg(current.conf, upper.conf) map { rb => rebuild(translate(rb)) }
        rollbacks.toList ++ rebuildings.toList
      case None =>
        List()
    }
  }
}