package mrsc.sll

import mrsc._
import mrsc.sll.SLLExpressions._
import mrsc.sll.NSLLExpressions._

// generator of residual programs in NSLL
// It generate a correct program for graphs where folding links to the upper node in the same path.
// TODO: ensure that all cases of folding are considered here
class NSLLResiduator(val tree: Graph[Expr, Contraction[Expr], Extra]) {

  private val sigs = scala.collection.mutable.Map[Path, (String, List[NVar])]()
  lazy val result = fixNames(fold(tree.root))

  // proceed base node or repeat node by creating letrec or call respectively 
  // otherwise, delegate to make
  private def fold(n: Node[Expr, Contraction[Expr], Extra]): NExpr = n.base match {

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

      val sub = findSubst(fnode.conf, n.conf)
      val sub1 = sub map { case (k, v) => (NVar(k.name), convert(v)) }
      NCall(name, args map { nSubst(_, sub1) })
    }
  }

  private def make(n: Node[Expr, Contraction[Expr], Extra]): NExpr = n.conf match {
    case Var(vn) => NVar(vn)
    case Ctr(cn, _) => NCtr(cn, n.outs.map { _.node }.map(fold))
    case Let(_, bs) => {
      val n0 :: ns = n.outs.map { _.node }
      val sub = Map() ++ (bs map { kv => NVar(kv._1.name) } zip (ns map fold))
      nSubst(fold(n0), sub)
    }
    case _ =>
      if (n.outs.head.driveInfo == null) {
        // transient step
        fold(n.outs.head.node)
      } else {
        // variants
        val sortedChildren = n.outs sortWith { (n1, n2) => (n1.driveInfo.pat.asInstanceOf[Ctr].name compareTo n2.driveInfo.pat.asInstanceOf[Ctr].name) < 0 }
        val sel = NVar(n.outs.head.driveInfo.v)
        val bs = sortedChildren map { c => (convertPat(c.driveInfo.pat.asInstanceOf[Ctr]), fold(c.node)) }
        NCase(sel, bs)
      }
  }

  private def createSignature(fNode: Node[Expr, Contraction[Expr], Extra], recNodes: List[Node[Expr, Contraction[Expr], Extra]]): (String, List[NVar]) = {
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

// Can create a correct program for any (almost any :)) graph
class SLLResiduator(val tree: Graph[Expr, Contraction[Expr], Extra]) {
  type Sig = (String, List[Var])
  private val sigs = scala.collection.mutable.Map[Path, Sig]()
  private val defs = new scala.collection.mutable.ListBuffer[Def]
  lazy val result = (fold(tree.root), Program(defs.toList))

  private def fold(n: Node[Expr, Contraction[Expr], Extra]): Expr = n.base match {

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
      val sub = findSubst(fnode.conf, n.conf)
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

  private def make(n: Node[Expr, Contraction[Expr], Extra], sig: Option[Sig]): Expr = n.conf match {
    case Var(vn) => Var(vn)
    case Ctr(cn, _) => {
      // TODO
      Ctr(cn, n.outs.map { _.node }.map(fold))
    }
    case Let(_, bs) => {
      val n0 :: ns = n.outs.map { _.node }
      val body = fold(n0)
      val sub = Map() ++ (bs map { kv => Var(kv._1.name) } zip (ns map fold))
      subst(body, sub)
    }
    case _ =>
      if (n.outs.head.driveInfo == null) {
        // transient step
        lazy val traversed = fold(n.outs.head.node)
        for ((fname, fargs) <- sig)
          defs += FFun(fname, fargs, traversed)
        traversed
      } else {
        val sig1 @ (gname, gargs) = sig.getOrElse(createSignature(n))
        sigs(n.path) = sig1
        for (out <- n.outs)
          defs += GFun(gname, toPat(out.driveInfo.pat.asInstanceOf[Ctr]), gargs.tail, fold(out.node))
        GCall(gname, gargs)
      }
  }

  private def toPat(p: Ctr): Pat = Pat(p.name, p.args map { case v: Var => v })

  private def createSignature(fNode: Node[Expr, Contraction[Expr], Extra]): (String, List[Var]) = {
    var fVars: List[Var] = vars(fNode.conf)
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