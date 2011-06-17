package mrsc.sll

import mrsc._
import mrsc.sll.SLLExpressions._

class NaiveResiduator extends Residuation[Expr, Expr] {

  private val sigs = scala.collection.mutable.Map[Path, (String, List[Var])]()

  override def residuate(tree: Graph[Expr, DriveInfo[Expr], _]): Expr = {

    def fold(n: Node[Expr, DriveInfo[Expr], _]): Expr = n.base match {

      case None =>
        lazy val traversed = build(n)
        val repeatNodes = tree.leaves.filter { _.base == Some(n.path) }

        if (repeatNodes.isEmpty) {
          traversed
        } else {
          val (f, vars) = createSignature(n)
          sigs(n.path) = (f, vars)
          val newVars = vars // map { p => createVar() }
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

    def build(n: Node[Expr, DriveInfo[Expr], _]): Expr =
      if (n.isLeaf) {
        return n.conf
      } else {
        val children @ (n1 :: ns) = n.outs
        n1.driveInfo match {
          case TransientStepInfo =>
            fold(n1.node)
          case DecomposeStepInfo(compose) =>
            val ch1 = children.map { out => fold(out.node) }
            val e1 = compose(ch1)
            e1
            
          case VariantsStepInfo(c) =>
            val fname = createFName()
            val vars = SLLExpressions.vars(n.conf)
            val vars1 = vars remove {_ == Var(c.v)}
            val branches = children map { n2 =>
              val VariantsStepInfo(Contraction(v, pat)) = n2.driveInfo
              GFun(fname, toPat(pat.asInstanceOf[Ctr]), vars1, fold(n2.node))
            }
            val sortedBranches = branches.sortBy(_.p.name)
            val call = GCall(fname, Var(c.v) :: vars1 )
            Where(call, branches)
        }
      }

    fixNames(fold(tree.root))
  }

  private def createSignature(fNode: Node[Expr, DriveInfo[Expr], _]): (String, List[Var]) = {
    var fVars: List[Var] = vars(fNode.conf)
    (createFName(), fVars)
  }

  var fCount = 0
  private def createFName(): String = {
    fCount = fCount + 1
    "f." + fCount
  }

  private def toPat(p: Ctr): Pat = Pat(p.name, p.args map { case v: Var => v })
}