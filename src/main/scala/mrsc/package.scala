package object mrsc {
  type Path = List[Int]
  type CoPath = List[Int]

  type Loopback = Option[Path]

  type Nodes[C, I] = List[Node[C, I]]
  type CoNodes[C, I] = List[CoNode[C, I]]
  
  type Edges[C, I] = List[Edge[Node[C, I], I]]
}