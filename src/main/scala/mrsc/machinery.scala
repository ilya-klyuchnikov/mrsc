package mrsc

// This is for standard semantics
case class Contraction[+C](v: Name, pat: C) {
  override def toString =
    if (v != null) v + " = " + pat else ""
}

abstract sealed trait DriveStep[+C]
case class TransientDriveStep[C](next: C) extends DriveStep[C]
case object StopDriveStep extends DriveStep[Nothing]
case class DecomposeDriveStep[C](compose: List[C] => C, parts: List[C]) extends DriveStep[C]
case class VariantsDriveStep[C](cases: List[(Contraction[C], C)]) extends DriveStep[C]

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

sealed trait Extra
case object NoExtra extends Extra

/*!# Modeling expectations
 */

/*! The following exception usually means that some modeling expectation (or hypothesis) 
 was not met during actual modeling.  
 */
class ModelingError(val message: String) extends Exception(message: String)

/*!# Whistle and tricks
  
 `GenericMultiMachine` is well suited for implementing different aspects in traits.
  
 It turns out that "pure" multi-result supercompilation is limited by whistle only. 
 "Advanced" (such as two-level) multi-result supercompilation is limited by additional tricks 
 (such as improvement lemma) applied during supercompilation. 
  
 So `W` here stands for "whistle signal".
*/
trait GenericMultiMachine[C, D, E] extends Machine[C, D, E] {

  type W = Option[CoNode[C, D, E]]
  def isLeaf(pState: PState[C, D, E]): Boolean
  def fold(pState: PState[C, D, E]): Option[Path]
  def blame(pState: PState[C, D, E]): W
  def drive(whistle: W, pState: PState[C, D, E]): List[Command[C, D, E]]
  def rebuildings(whistle: W, pState: PState[C, D, E]): List[Command[C, D, E]]
  def tricks(whistle: W, pState: PState[C, D, E]): List[Command[C, D, E]] = List()

  /*! The logic of this machine is straightforward:
     
     * If there are opportunities for folding, lets fold.
     
     * Otherwise, lets try all variants.
    
   Note that the whistle signal is passed to `drive`, `rebuildings` and `tricks`.
  */
  override def steps(pState: PState[C, D, E]): List[Command[C, D, E]] =
    if (isLeaf(pState))
      List(ConvertToLeaf)
    else fold(pState) match {
      case Some(path) =>
        List(MakeFold(path))
      case _ =>
        val signal = blame(pState)
        val driveSteps = drive(signal, pState)
        val genSteps = rebuildings(signal, pState)
        val trickySteps = tricks(signal, pState)
        driveSteps ++ (trickySteps ++ genSteps)
    }
}

// doesn't not care about whistle signals 
trait Driving[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with OperationalSemantics[C] {
  override def drive(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] =
    drive(pState.current.conf) match {
      case StopDriveStep =>
        List()
      case DecomposeDriveStep(compose, args) =>
        val stepInfo = DecomposeStepInfo(compose)
        val subSteps = args map { a => ChildNode(a, stepInfo, NoExtra) }
        List(AddChildNodes(subSteps))
      case TransientDriveStep(next) =>
        val subSteps = List(ChildNode(next, TransientStepInfo, NoExtra))
        List(AddChildNodes(subSteps))
      case VariantsDriveStep(cases) =>
        val subSteps = cases map { case (contr, next) => ChildNode(next, VariantsStepInfo(contr), NoExtra) }
        List(AddChildNodes(subSteps))
    }

  override def isLeaf(pState: PState[C, DriveInfo[C], Extra]) =
    !isDrivable(pState.current.conf)
}

trait RuleDriving[C] extends GenericMultiMachine[C, Int, Extra] with RewriteSemantics[C] {
  override def drive(whistle: W, pState: PState[C, Int, Extra]): List[Command[C, Int, Extra]] =
    whistle match {
      case Some(blamed) => List(DiscardGraph)
      case None =>
        val subSteps =
          for ((next, i) <- drive(pState.current.conf).zipWithIndex if next.isDefined)
            yield ChildNode(next.get, i + 1, NoExtra)
        List(AddChildNodes(subSteps))
    }

  override def isLeaf(pState: PState[C, Int, Extra]) =
    false
}

trait SimpleDriving[C] extends Driving[C] {
  override def drive(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] =
    whistle match {
      case Some(blamed) => List()
      case None => super.drive(whistle, pState)
    }
}

trait PruningDriving[C] extends Driving[C] {
  override def drive(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] =
    whistle match {
      case Some(blamed) => List(DiscardGraph)
      case None => super.drive(whistle, pState)
    }
}

trait RenamingFolding[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with Syntax[C] {
  override def fold(pState: PState[C, DriveInfo[C], Extra]): Option[Path] =
    pState.current.ancestors.find { n => instance.equiv(pState.current.conf, n.conf) } map { _.path }
}

trait InstanceFolding[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with Syntax[C] {
  override def fold(pState: PState[C, DriveInfo[C], Extra]): Option[Path] =
    pState.current.ancestors.find { n => instance.lteq(n.conf, pState.current.conf) } map { _.path }
}

trait SimpleInstanceFolding[C, D] extends GenericMultiMachine[C, D, Extra] with PreSyntax[C] {
  override def fold(pState: PState[C, D, Extra]): Option[Path] =
    pState.current.ancestors.find { n => instance.lteq(n.conf, pState.current.conf) } map { _.path }
}

trait SimpleInstanceFoldingToAny[C, D] extends GenericMultiMachine[C, D, Extra] with PreSyntax[C] {
  override def fold(pState: PState[C, D, Extra]): Option[Path] =
    pState.complete.find { n => instance.lteq(n.conf, pState.current.conf) } map { _.path }
}

trait NoTricks[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] {
  override def tricks(w: W, pState: PState[C, DriveInfo[C], Extra]) =
    Nil
}

// Ordering-based termination
trait BinaryWhistle[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] {
  val ordering: PartialOrdering[C]
  override def blame(pState: PState[C, DriveInfo[C], Extra]): W =
    pState.current.ancestors find { n => ordering.lteq(n.conf, pState.current.conf) }
}

trait UnaryWhistle[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] {
  def unsafe(c: C): Boolean
  override def blame(pState: PState[C, DriveInfo[C], Extra]): W =
    if (unsafe(pState.current.conf)) Some(pState.current) else None
}

trait SimpleUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Extra] {
  def unsafe(c: C): Boolean
  override def blame(pState: PState[C, D, Extra]): W =
    if (unsafe(pState.current.conf)) Some(pState.current) else None
}

// NOW Generalization!
trait AlwaysCurrentGens[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with Syntax[C] {
  override def rebuildings(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] = {
    rawRebuildings(pState.current.conf) map translate map { ReplaceNode(_, NoExtra) }
  }
}

trait CurrentGensOnWhistle[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with Syntax[C] {
  override def rebuildings(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] = {
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val replaces =
          rawRebuildings(pState.current.conf) map translate map { ReplaceNode(_, NoExtra) }
        replaces
    }
  }
}

trait SimpleCurrentGensOnWhistle[C, D] extends GenericMultiMachine[C, D, Extra] with PreSyntax[C] with SimpleUnaryWhistle[C, D] {
   override def rebuildings(whistle: W, pState: PState[C, D, Extra]): List[Command[C, D, Extra]] = {
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val rbs = rebuildings(pState.current.conf) filterNot unsafe
        rbs map { ReplaceNode(_, NoExtra) }
    }
  }
}

trait CurrentGensOnUnaryWhistle[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with Syntax[C] with UnaryWhistle[C] {
  override def rebuildings(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] = {
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val rbs =
          rawRebuildings(pState.current.conf) map translate filterNot unsafe
        rbs map { ReplaceNode(_, NoExtra) }
    }
  }
}

trait AlwaysCurrentGensWithUnaryWhistle[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with Syntax[C] with UnaryWhistle[C] {
  override def rebuildings(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] = {
    val rbs = rawRebuildings(pState.current.conf) map translate filterNot unsafe
    rbs map { ReplaceNode(_, NoExtra) }
  }
}

trait SimpleGensWithUnaryWhistle[C, D] extends GenericMultiMachine[C, D, Extra] with PreSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: W, pState: PState[C, D, Extra]): List[Command[C, D, Extra]] = {
    val rbs = rebuildings(pState.current.conf) filterNot unsafe
    rbs map { ReplaceNode(_, NoExtra) }
  }
}

trait BlamedGensOnWhistle[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with Syntax[C] {
  override def rebuildings(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] = {
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val rollbacks =
          rawRebuildings(blamed.conf) map translate map { RollbackSubGraph(blamed, _, NoExtra) }
        rollbacks
    }
  }
}

trait AllGensOnWhistle[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with Syntax[C] {
  override def rebuildings(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] = {
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val replaces =
          rawRebuildings(pState.current.conf) map translate map { ReplaceNode(_, NoExtra) }
        val rollbacks =
          rawRebuildings(blamed.conf) map translate map { RollbackSubGraph(blamed, _, NoExtra) }
        replaces ++ rollbacks
    }
  }
}

trait MSGBlamedOrSplitCurrent[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with NaiveMSG[C] {

  def rebuildings(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] = {
    whistle match {
      case Some(blamed) =>
        val currentConf = pState.current.conf
        val blamedConf = blamed.conf
        //println(blamedConf + " < " + currentConf)
        msg(blamedConf, currentConf) match {
          // try MSG
          case Some(rb) =>
            val conf1 = translate(rb)
            //println("rollback " + conf1)
            val rollback = RollbackSubGraph(blamed, conf1, NoExtra)
            List(rollback)
          // If there is no msg, then just split the down configuration
          case None =>
            // splitting the down configuration
            val cands = rawRebuildings(currentConf)
            val cands1 = cands filter { case (c1, _) => cands forall { case (c2, _) => instance.lteq(c1, c2) } }
            val let = translate(cands1(0))
            //println("replace " + let)
            val replace = ReplaceNode(let, NoExtra)
            List(replace)
        }
      case None =>
        List()
    }
  }
}

trait MSGCurrentOrSplitBlamed[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with NaiveMSG[C] {

  def rebuildings(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] = {
    whistle match {
      case Some(blamed) =>
        val currentConf = pState.current.conf
        val blamedConf = blamed.conf
        //println(blamedConf + " < " + currentConf)
        msg(currentConf, blamedConf) match {
          // try MSG
          case Some(rb) =>
            val conf1 = translate(rb)
            //println("replace " + conf1)
            val replace = ReplaceNode(conf1, NoExtra)
            List(replace)
          // If there is no msg, then just split the up configuration
          case None =>
            // splitting the down configuration
            val cands = rawRebuildings(blamedConf)
            val cands1 = cands filter { case (c1, _) => cands forall { case (c2, _) => instance.lteq(c1, c2) } }
            val let = translate(cands1(0))
            //println("rollback " + let)
            val rollback = RollbackSubGraph(blamed, let, NoExtra)
            List(rollback)
        }
      case None =>
        List()
    }
  }
}

trait MixMsg[C] extends GenericMultiMachine[C, DriveInfo[C], Extra] with NaiveMSG[C] {

  def rebuildings(whistle: W, pState: PState[C, DriveInfo[C], Extra]): List[Command[C, DriveInfo[C], Extra]] = {
    whistle match {
      case Some(blamed) =>
        val currentConf = pState.current.conf
        val blamedConf = blamed.conf
        val replace = msg(currentConf, blamedConf) map { msg1 =>
          ReplaceNode(translate(msg1), NoExtra)
        }
        val rollback = msg(blamedConf, currentConf) map { msg1 =>
          RollbackSubGraph(blamed, translate(msg1), NoExtra)
        }
        rollback.toList ++ replace.toList
      case None =>
        List()
    }
  }
}