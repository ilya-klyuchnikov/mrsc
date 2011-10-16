package mrsc.pfp

import mrsc.core._

/*!# Equivalence and subclass relations on configurations
  
  If the current configuration is `equivalent` to another configuration labeling
  an ancestor node, the supercompiler can loop back to that ancestor.
  (Looping back to arbitrary completed nodes is also possible if the operation
  `rollback` is not used, so that non-ancestor nodes cannot be pruned.)
  If the current configuration is an `instance` of another configuration
  the supercompiler may perform some actions in order to make looping
  back possible.
  
  At the semantic level a configuration c is regarded as a representation of set(c),
  a set of states of a computation process.
  It is assumed that
    (1) subclass.equiv(c1, c2) implies that set(c1) = set(c2);
    (2) subclass.lteq(c1, c2) implies set(c1) <= set(c2).
    
  When considering configurations as sets, the relation subclass in general is undecidable.
  However, in PFP setting we consider this relation as being total: syntactic approximation
  is used usually in traditional supercompilers.
 */
trait PFPSyntax[C] {
  def subst(c: C, sub: Subst[C]): C
  def findSubst(from: C, to: C): Option[Subst[C]]
  def rawRebuildings(c: C): List[RawRebuilding[C]]
  def translate(rebuilding: RawRebuilding[C]): C
  def trivialRb(c: C)(rb: RawRebuilding[C]) =
    (rb._2.values.toSet + rb._1) exists {subclass.equiv(c, _)}
  def rebuildings(c: C): List[C] =
    rawRebuildings(c) filterNot trivialRb(c) map translate
  def size(c: C): Int
  val subclass: PartialOrdering[C]
}

trait PFPSemantics[C] {
  def driveConf(c: C): DriveStep[C]
}

trait Residuation[C] {
  def residuate(graph: TGraph[C, DriveInfo[C]]): C
}

trait MSG[C] extends PFPSyntax[C] {
  def msg(c1: C, c2: C): Option[RawRebuilding[C]] = {
    val nonTrivialRbs = rawRebuildings(c1) filterNot trivialRb(c1)
    val sharedRbs = nonTrivialRbs filter { rb => subclass.gteq(rb._1, c2) }
    sharedRbs find { rb => sharedRbs forall { other => subclass.lteq(rb._1, other._1) } }
  }
}

trait MutualGens[C] extends PFPSyntax[C] {
  def mutualGens(c1: C, c2: C): List[RawRebuilding[C]] = {
    val nonTrivialRbs = rawRebuildings(c1) filterNot trivialRb(c1)
    nonTrivialRbs filter { rb => subclass.gteq(rb._1, c2) }
  } 
}