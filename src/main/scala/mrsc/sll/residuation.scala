package mrsc.sll

import mrsc._
import mrsc.sll.SLLExpressions._

/*! `SLLResiduator` residuates a graph of configuration into 
     a 'where'-expression. Where-expression is a bunch of embedded 
     function definitions. This Residuator assumes that the base configuration
     is an ancestor of repeated configurations. 
     Also this residuator doesn't perform full lifting, but performs limited
     lifting for g-functions (doesn't introduce repeated variables).
     Also there is no recursive g-calls: so we duplicate code a bit here.
     
     BTW, it is easier to write residuator if output language is just RULES:
     	lhs -> rhs
     you do not need to invent patterns, ..., so it is possible to write a generic
     component for residuation. 
     
     Note, that this code is purely functional: no vars, no var generation.
 */
object SLLResiduator extends Residuation[Expr] {

  override def residuate(graph: Graph[Expr, DriveInfo[Expr], _]): Expr =
    SyntaxNormalization.fixNames(fold(graph, graph.root))

  def fold(graph: Graph[Expr, DriveInfo[Expr], _], n: Node[Expr, DriveInfo[Expr], _]): Expr = n.base match {
    // base node
    case None if (graph.leaves.exists { _.base == Some(n.path) }) =>
      val traversed = build(graph, n)
      val (f, vars) = signature(n)
      val call = FCall(f, vars)
      val fdef = FFun(f, vars map { _.name }, traversed)
      Where(call, List(fdef))
    // repeat node
    case Some(fpath) =>
      val fnode = graph.get(fpath)
      val (name, args) = signature(fnode)
      val call = FCall(name, args)
      subst(call, findSubst(fnode.conf, n.conf).get)
    // other
    case _ => build(graph, n)
  }

  def build(tree: Graph[Expr, DriveInfo[Expr], _], n: Node[Expr, DriveInfo[Expr], _]): Expr =
    if (n.isLeaf) {
      return n.conf
    } else {
      val children @ (n1 :: ns) = n.outs
      n1.driveInfo match {
        case TransientStepInfo =>
          fold(tree, n1.node)
        case DecomposeStepInfo(compose) =>
          compose(children.map { out => fold(tree, out.node) })
        case VariantsStepInfo(_) =>
          val (fname, vs @ (v :: vars1)) = gSignature(n)
          val branches = children map {
            case Edge(n1, driveInfo) =>
              val VariantsStepInfo(Contraction(v, c @ Ctr(cn, _))) = driveInfo
              val pat = Pat(cn, vars(c) map { _.name })
              GFun(fname, pat, vars1 map { _.name }, fold(tree, n1))
          } sortBy (_.p.name)
          val call = GCall(fname, vs)
          Where(call, branches)
      }
    }

  private def signature(node: Node[Expr, DriveInfo[Expr], _]): (String, List[Var]) = {
    val fname = "f/" + node.path.mkString("/")
    (fname, vars(node.conf))
  }
  private def gSignature(node: Node[Expr, DriveInfo[Expr], _]): (String, List[Var]) = {
    val fname = "g/" + node.path.mkString("/")
    (fname, vars(node.conf))
  }

}