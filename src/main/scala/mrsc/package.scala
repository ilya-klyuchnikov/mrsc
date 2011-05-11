
/*! Just shortcuts for types.
 */
package object mrsc {
  type Path = List[Int]
  type CoPath = List[Int]
  type Loopback = Option[Path]
  type Nodes[C, D, E] = List[Node[C, D, E]]
  type CoNodes[C, D, E] = List[CoNode[C, D, E]]
  type Out[C, D, E] = Edge[Node[C, D, E], D]
  type Outs[C, D, E] = List[Out[C, D, E]]
  type In[C, D, E] = Edge[CoNode[C, D, E], D]
  type Name = String
}