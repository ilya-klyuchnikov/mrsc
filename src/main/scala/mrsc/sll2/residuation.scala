package mrsc.sll

import mrsc._
import mrsc.sll.SLLExpressions._

class NaiveResiduator extends Residuator[Expr, Expr] {

  private val sigs = scala.collection.mutable.Map[Path, (String, List[Var])]()

  override def residuate(tree: Graph[Expr, SubStepInfo[Expr], _]): Expr = {

    def fold(n: Node[Expr, SubStepInfo[Expr], _]): Expr = n.base match {

      case None =>
        lazy val traversed = build(n)
        val repeatNodes = tree.leaves.filter { _.base == Some(n.path) }

        if (repeatNodes.isEmpty) {
          traversed
        } else {
          val (f, vars) = createSignature(n)
          sigs(n.path) = (f, vars)
          val newVars = vars map { p => createVar() }
          val sub = (vars zip newVars).toMap
          val rhs = subst(traversed, sub)
          val fun = FFun(f, newVars, rhs)
          val recCall = FCall(f, vars)
          Where(recCall, List(fun))
        }

      case Some(fpath) =>
        val (name, args) = sigs(fpath)
        val fnode = tree.get(fpath)
        val sub = findSubst(fnode.conf, n.conf)
        val recCall = FCall(name, args map { subst(_, sub) })
        recCall
    }

    def build(n: Node[Expr, SubStepInfo[Expr], _]): Expr =
      if (n.isLeaf) {
        return n.conf
      } else {
        val children @ (n1 :: ns) = n.outs
        n1.driveInfo match {
          case TransientStep =>
            fold(n1.node)
          case DecomposeStep(compose) =>
            compose(children.map { out => fold(out.node) })
          case VariantBranchStep(c) =>
            val fname = createFName()
            val branches = children map { n2 =>
              val VariantBranchStep(Contraction(_, pat)) = n2.driveInfo
              GFun(fname, toPat(pat.asInstanceOf[Ctr]), List(), fold(n2.node))
            }
            val sortedBranches = branches.sortBy(_.p.name)
            val call = GCall(fname, List(Var(c.v)))
            Where(call, branches)
        }
      }

    fold(tree.root)
  }

  private def createSignature(fNode: Node[Expr, SubStepInfo[Expr], _]): (String, List[Var]) = {
    var fVars: List[Var] = vars(fNode.conf)
    (createFName(), fVars)
  }

  var fCount = 0
  private def createFName(): String = {
    fCount = fCount + 1
    "_." + fCount
  }

  var vCount = 0
  private def createVar(): Var = {
    vCount = vCount + 1
    Var("_." + vCount)
  }

  private def toPat(p: Ctr): Pat = Pat(p.name, p.args map { case v: Var => v })
}