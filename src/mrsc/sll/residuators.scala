package mrsc.sll

import mrsc._
import mrsc.sll.SLLExpressions._
import mrsc.sll.NSLLExpressions._

// generator of residual programs in NSLL
// It generate a correct program for graphs where folding links to the upper node in the same path.
// TODO: ensure that all cases of folding are considered here
class NSLLResiduator(val tree: Graph[Expr, Contraction]) {

  private val sigs = scala.collection.mutable.Map[Path, (String, List[NVar])]()
  lazy val result = fixNames(fold(tree.root))

  // proceed base node or repeat node by creating letrec or call respectively 
  // otherwise, delegate to make
  private def fold(n: Node[Expr, Contraction]): NExpr = n.base match {

    case None => {
      lazy val traversed = make(n)
      tree.leaves.filter { _.base == Some(n.path) } match {

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
    }

    case Some(fpath) => {
      val (name, args) = sigs(fpath)
      val fnode = tree.get(fpath)

      val sub = findSubst(fnode.configuration, n.configuration)
      val sub1 = sub map { case (k, v) => (NVar(k.name), convert(v)) }
      NCall(name, args map { nSubst(_, sub1) })
    }
  }

  private def make(n: Node[Expr, Contraction]): NExpr = n.configuration match {
    case Var(vn) => NVar(vn)
    case Ctr(cn, _) => NCtr(cn, n.children.map(fold))
    case Let(_, bs) => {
      val n0 :: ns = n.children
      val sub = Map() ++ (bs map { kv => NVar(kv._1.name) } zip (ns map fold))
      nSubst(fold(n0), sub)
    }
    case _ =>
      if (n.children.head.info == null) {
        // transient step
        fold(n.children.head)
      } else {
        // variants
        val sortedChildren = n.children sortWith { (n1, n2) => (n1.info.pat.name compareTo n2.info.pat.name) < 0 }
        val sel = NVar(n.children.head.info.v.name)
        val bs = sortedChildren map { c => (convert(c.info.pat), fold(c)) }
        NCase(sel, bs)
      }
  }

  private def createSignature(fNode: Node[Expr, Contraction], recNodes: List[Node[Expr, Contraction]]): (String, List[NVar]) = {
    var fVars: List[Var] = vars(fNode.configuration)
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

// Can create a correct program for any (almost any :)) graph
class SLLResiduator(val tree: Graph[Expr, Contraction]) {
  type Sig = (String, List[Var])
  private val sigs = scala.collection.mutable.Map[Path, Sig]()
  private val defs = new scala.collection.mutable.ListBuffer[Def]
  lazy val result = (fold(tree.root), Program(defs.toList))

  private def fold(n: Node[Expr, Contraction]): Expr = n.base match {

    case None => {
      tree.leaves.filter { _.base == Some(n.path) } match {
        case Nil =>
          make(n, None)
        case repeatNodes =>
          val sig @ (f, vars) = createSignature(n)
          sigs(n.path) = (f, vars)
          val newVars = vars map { p => createVar() }
          val sub = Map(vars zip newVars: _*)
          val body = subst(make(n, Some(sig)), sub)
          FCall(f, vars)
      }
    }

    case Some(fpath) => {
      val (name, args) = sigs(fpath)
      val fnode = tree.get(fpath)
      val sub = findSubst(fnode.configuration, n.configuration)
      val res = FCall(name, args map { subst(_, sub) })
      
      tree.leaves.filter { _.base == Some(n.path) } match {
        case Nil =>
          res
        case repeatNodes =>
          val sig @ (f, vars) = sigs.getOrElse(n.path, createSignature(n))
          sigs(n.path) = (f, vars)
          val newVars = vars map { p => createVar() }
          val sub = Map(vars zip newVars: _*)
          val body = res
          defs += FFun(f, vars, res)
          FCall(f, vars)
      }
    }
  }

  private def make(n: Node[Expr, Contraction], sig: Option[Sig]): Expr = n.configuration match {
    case Var(vn) => Var(vn)
    case Ctr(cn, _) => {
      // TODO
      Ctr(cn, n.children.map(fold))
    }
    case Let(_, bs) => {
      val n0 :: ns = n.children
      val body = fold(n0)
      val sub = Map() ++ (bs map { kv => Var(kv._1.name) } zip (ns map fold))
      subst(body, sub)
    }
    case _ =>
      if (n.children.head.info == null) {
        // transient step
        lazy val traversed = fold(n.children.head)
        for ((fname, fargs) <- sig)
          defs += FFun(fname, fargs, traversed)
        traversed
      } else {
        val sig1@(gname, gargs) = sig.getOrElse(createSignature(n))
        sigs(n.path) = sig1
        for (cn <- n.children)
          defs += GFun(gname, cn.info.pat, gargs.tail, fold(cn))
        GCall(gname, gargs)
      }
  }

  private def createSignature(fNode: Node[Expr, Contraction]): (String, List[Var]) = {
    var fVars: List[Var] = vars(fNode.configuration)
    (createFName(), fVars)
  }

  var fCount = 0
  private def createFName(): String = {
    fCount = fCount + 1
    "f." + fCount
  }

  var vCount = 0
  private def createVar(): Var = {
    vCount = vCount + 1
    Var("v." + vCount)
  }
}