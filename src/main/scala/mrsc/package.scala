/*! Just shortcuts for types.
 */
package object mrsc {
  type Path = List[Int]
  type CoPath = List[Int]

  type Name = String
  type Subst[C] = Map[Name, C]
  type Rebuilding[C] = (C, Subst[C])

  def emptyContraction[C] = Contraction[C](null, null.asInstanceOf[C])
  def emptySubst[C] = Map[Name, C]()
}