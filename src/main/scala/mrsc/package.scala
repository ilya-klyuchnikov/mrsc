package object mrsc {
  type Path = List[Int]
  type CoPath = List[Int]

  type Loopback = Option[Path]

  type Nodes[C, I] = List[Node[C, I]]
  type CoNodes[C, I] = List[CoNode[C, I]]
  
  type Out[C, I] = Edge[Node[C, I], I]
  type In[C, I] = Edge[CoNode[C, I], I]
}