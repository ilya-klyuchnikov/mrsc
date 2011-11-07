package mrsc

package object pfp {
  type Name = String
  type Subst[C] = Map[Name, C]
  type RawRebuilding[C] = (C, Subst[C])
  type PFPRules[C] = MultiResultSCRules[C, DriveInfo[C]]

  def emptyContraction[C] = Contraction[C](null, null.asInstanceOf[C])
  def emptySubst[C] = Map[Name, C]()
}