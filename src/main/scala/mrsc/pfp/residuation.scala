package mrsc.pfp

import mrsc.core._

case class Residuator(val g: TGraph[Term, DeforestStep]) {

  val result: Term = residuate(g.root)

  // this should be very cool method
  // we should pass context into this call
  // context will grow as we proceed base nodes
  // and generalization nodes
  // also we should change deforestation rules in order to produce
  // the descriptive head node (the leftmost child)
  def residuate(node: TNode[Term, DeforestStep]): Term = node.conf match {
    case Ctr(n, _) =>
      val args: List[Field] = node.outs.map { case TEdge(child, CtrArg(l)) => (l, residuate(child)) }
      Ctr(n, args)
    case v@FVar(n) =>
      v
    case _ =>
      null
  }
}