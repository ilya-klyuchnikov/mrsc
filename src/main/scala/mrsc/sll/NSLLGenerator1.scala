package mrsc.sll

import mrsc._
import mrsc.sll.SLLExpressions._
import mrsc.sll.NSLLExpressions._

// Generator for new types of information in graphs (SubStepInfo)
//
// generator of residual programs in NSLL
// It generate a correct program for graphs where folding links to the upper node in the same path.
// TODO: ensure that all cases of folding are considered here
class NSLLResiduator2(val tree: Graph[Expr, SubStepInfo[Expr], Extra]) {

  private val sigs = scala.collection.mutable.Map[Path, (String, List[NVar])]()
  lazy val result = fixNames(fold(tree.root))
  private val extractCases = true

  // proceed base node or repeat node by creating letrec or call respectively 
  // otherwise, delegate to make
  private def fold(n: Node[Expr, SubStepInfo[Expr], Extra]): NExpr = n.base match {

    case None =>
      lazy val traversed = make(n)
      tree.leaves.filter { _.base == Some(n.path) } match {

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

  private def isCaseNode(n: Node[Expr, SubStepInfo[Expr], Extra]): Boolean = {
    if (n.isLeaf) {
      return false
    } else {
      val children @ (n1 :: ns) = n.outs
      n1.driveInfo.stepKind == Variants
    }
  }

  private def make(n: Node[Expr, SubStepInfo[Expr], Extra]): NExpr = {
    if (n.isLeaf) {
      return convert(n.conf)
    }

    val children @ (n1 :: ns) = n.outs

    n1.driveInfo.stepKind match {

      case Transient =>
        fold(n1.node)

      case CtrDecompose =>
        val ctrName = n.conf.asInstanceOf[Ctr].name
        NCtr(ctrName, children.map(out => fold(out.node)))

      case LetDecompose =>
        val body = fold(n1.node)
        val sub = ns.map { n2 => (NVar(n2.driveInfo.asInstanceOf[LetPartStep].v), fold(n2.node)) }.toMap
        nSubst(body, sub)

      case Generalization =>
        fold(n1.node)

      case Variants =>
        val sel = fold(n1.node)
        val branches = ns map { n2 =>
          val VariantBranchStep(Contraction(_, pat)) = n2.driveInfo
          (convertPat(pat.asInstanceOf[Ctr]), fold(n2.node))
        }
        val sortedBranches = branches.sortBy(_._1.name)
        NCase(sel, sortedBranches)
    }

  }

  private def createSignature(fNode: Node[Expr, SubStepInfo[Expr], Extra], recNodes: List[Node[Expr, SubStepInfo[Expr], Extra]]): (String, List[NVar]) = {
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