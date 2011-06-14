package mrsc.sll

import mrsc._
import mrsc.sll.SLLExpressions._
import mrsc.sll.NSLLExpressions._

// Generator for new types of information in graphs (DriveInfo)
//
// generator of residual programs in NSLL
// It generate a correct program for graphs where folding links to the upper node in the same path.
// TODO: ensure that all cases of folding are considered here
class NSLLResiduator2(val tree: Graph[Expr, DriveInfo[Expr], Extra]) {

  private val sigs = scala.collection.mutable.Map[Path, (String, List[NVar])]()
  lazy val result = fixNames(fold(tree.root))
  private val extractCases = false

  // proceed base node or repeat node by creating letrec or call respectively 
  // otherwise, delegate to make
  private def fold(n: Node[Expr, DriveInfo[Expr], Extra]): NExpr = n.base match {

    case None =>
      lazy val traversed = make(n)
      tree.leaves.filter { _.base == Some(n.path) } match {

        // this is ad-hoc hack
        // this is a bit wrong: we need to simplify graph first
        case Nil if extractCases && isCaseNode(n) =>
          val (f, vars) = createSignature(n, Nil)
          sigs(n.path) = (f, vars)
          val newVars = vars map { p => createVar() }
          val sub = Map(vars zip newVars: _*)
          val body = nSubst(traversed, sub)
          NLet(f, NFun(f, newVars, body), NCall(f, vars))

        case Nil =>
          traversed

        case repeatNodes =>
          val (f, vars) = createSignature(n, repeatNodes)
          sigs(n.path) = (f, vars)
          val newVars = vars map { p => createVar() }
          val sub = Map(vars zip newVars: _*)
          val body = nSubst(traversed, sub)
          NLet(f, NFun(f, newVars, body), NCall(f, vars))

      }

    case Some(fpath) =>
      val (name, args) = sigs(fpath)
      val fnode = tree.get(fpath)

      val sub = findSubst(fnode.conf, n.conf)
      val sub1 = sub map { case (k, v) => (NVar(k.name), convert(v)) }
      NCall(name, args map { nSubst(_, sub1) })
  }

  import StepKind._

  private def isCaseNode(n: Node[Expr, DriveInfo[Expr], Extra]): Boolean = {
    if (n.isLeaf) {
      return false
    } else {
      val children @ (n1 :: ns) = n.outs
      n1.driveInfo.stepKind == Variants
    }
  }

  private def make(n: Node[Expr, DriveInfo[Expr], Extra]): NExpr = {
    if (n.isLeaf) {
      return convert(n.conf)
    }

    val children @ (n1 :: ns) = n.outs

    n1.driveInfo.stepKind match {

      case Transient =>
        fold(n1.node)

      case Decompose =>
        // TODO: fix it
        val ds = n1.driveInfo.asInstanceOf[DecomposeStep[NExpr]]
        ds.compose(children.map(out => fold(out.node)))
        null

      case Variants =>
        val branches = children map { n2 =>
          val VariantBranchStep(Contraction(_, pat)) = n2.driveInfo
          (convertPat(pat.asInstanceOf[Ctr]), fold(n2.node))
        }
        val sortedBranches = branches.sortBy(_._1.name)
        NCase(NVar(n1.driveInfo.asInstanceOf[VariantBranchStep[Expr]].contr.v), sortedBranches)
    }

  }

  private def createSignature(fNode: Node[Expr, DriveInfo[Expr], Extra], recNodes: List[Node[Expr, DriveInfo[Expr], Extra]]): (String, List[NVar]) = {
    var fVars: List[Var] = vars(fNode.conf)
    (createFName(), fVars map { v => NVar(v.name) })
  }

  var fCount = 0
  private def createFName(): String = {
    fCount = fCount + 1
    "_." + fCount
  }

  var vCount = 0
  private def createVar(): NVar = {
    vCount = vCount + 1
    NVar("_." + vCount)
  }
}