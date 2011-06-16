/*! Just shortcuts for types.
 */
package object mrsc {
  type Path = List[Int]
  type CoPath = List[Int]
  type Loopback = Option[Path]
  type Nodes[C, D, E] = List[Node[C, D, E]]
  type CoNodes[C, D, E] = List[CoNode[C, D, E]]
  
  type Name = String
  type Subst[C] = Map[Name, C]
  type Rebuilding[C] = (C, Subst[C])
}