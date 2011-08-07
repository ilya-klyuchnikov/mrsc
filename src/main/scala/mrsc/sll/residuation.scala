package mrsc.sll

import mrsc.core._
import mrsc.pfp._
import mrsc.sll.SLLSyntax._

/*! `SLLResiduator` residuates a graph of configuration into 
     a 'where'-expression. Where-expression is a bunch of embedded 
     function definitions. 
     We do not create recursive g-calls: so we duplicate code a bit here.
     This residuator assumes full positive information propagation
     (doesn't duplicate variable when introducing g-functions).
     
     BTW, it is easier to write residuator if output language is just RULES:
     	lhs -> rhs
     you do not need to invent patterns, ..., so it is possible to write a generic
     component for residuation. 
     
     Note, that this code is purely functional: no vars.
 */
object SLLResiduator extends Residuation[Expr] {

  override def residuate(graph: Graph[Expr, DriveInfo[Expr], _]): Expr =
    SyntaxNormalization.fixNames(fold(graph, graph.root))

  def fold(graph: Graph[Expr, DriveInfo[Expr], _], n: Node[Expr, DriveInfo[Expr], _]): Expr = n.base match {
    // base node
    case None if (graph.leaves.exists { _.base == Some(n.path) }) =>
      val (f, vars) = signature(n)
      val call = FCall(f, vars)
      val body = build(graph, n)
      val fdef = FFun(f, vars map { _.name }, body)
      Where(call, List(fdef))
    // repeat node
    case Some(fpath) =>
      val (f, vars) = signature(graph.get(fpath))
      val call = FCall(f, vars)
      val fnode = graph.get(fpath)
      subst(call, findSubst(fnode.conf, n.conf).get)
    // transient reduction
    case _ => build(graph, n)
  }

  def build(tree: Graph[Expr, DriveInfo[Expr], _], n: Node[Expr, DriveInfo[Expr], _]): Expr = n.outs match {
    case Nil => n.conf
    case children @ (n1 :: ns) => n1.driveInfo match {
      case TransientStepInfo =>
        fold(tree, n1.node)
      case DecomposeStepInfo(compose) =>
        compose(children map { _.node } map { fold(tree, _) })
      case VariantsStepInfo(_) =>
        val (fname, vs @ (v :: vars1)) = gSignature(n)
        val branches = children map { e =>
          val VariantsStepInfo(Contraction(v, c @ Ctr(cn, _))) = e.driveInfo
          val pat = Pat(cn, vars(c) map { _.name })
          GFun(fname, pat, vars1 map { _.name }, fold(tree, e.node))
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