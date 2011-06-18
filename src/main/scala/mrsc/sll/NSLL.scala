package mrsc.sll

import scala.text.Document
import scala.text.Document._

// It seems that the easiest elegant way 
// to normalize programs is to use explicit
// letrecs and case-expressions.
// 
// It is an interesting question whether we can "normalize"
// programs in elegant way using SLL language
//
// So NLanguage is a "normalized" SLL

object NSLL {
  val ED: scala.text.Document = empty
  def bToDoc(x: (NPat, NExpr)): Document = group(x._1.toDoc :: " ->" :: nest(2, ED :/: x._2.toDoc :: ";" :: ED))

  import scala.collection.mutable.{ ListBuffer, Set => MSet }

  // transforms nsll to sll
  // assumptions: nsll is without lambda-dropping + 
  // all case expressions are wrapped into let-expressions
  // "good" case-expressions
  def toSLL(ne: NExpr): SLLTask = {
    val defs: ListBuffer[Def] = ListBuffer()
    val gs = MSet[String]()

    def traverse(ne: NExpr): Expr = ne match {
      case NVar(n) =>
        Var(n)
      case NCtr(n, args) =>
        Ctr(n, args map traverse)
      case NCall(n, args) if gs(n) =>
        GCall(n, args map traverse)
      case NCall(n, args) =>
        FCall(n, args map traverse)
      case NLet(n, nfun, in) =>
        traverseFun(nfun)
        traverse(in)
      case _ => throw new Error("unexpected expression: " + ne)
    }

    def traverseFun(nfun: NFun): Unit = nfun.term match {
      case NCase(sel, bs) =>
        gs += nfun.name
        val sel1 = sel.asInstanceOf[NVar]
        // TODO: decide what to do with substitution
        for ((NPat(n, vars), e1) <- bs) {
          val p1 = Pat(n, vars map { v => v.n })
          val c1 = Ctr(n, vars map { v => Var(v.n) })
          val body = traverse(e1)
          val correctBody = SLLExpressions.subst(body, Map(sel1.n -> c1))
          val g = GFun(nfun.name, p1, nfun.args.tail map { v => v.n }, correctBody)
          defs += g
        }

      case e =>
        val f = FFun(nfun.name, nfun.args map { v => v.n }, traverse(nfun.term))
        defs += f
    }

    val goal1 = traverse(ne)
    val sorted = defs.sortWith { (d1, d2) =>
      if (d1.name < d2.name) {
        true
      } else if (d1.name > d2.name) {
        false
      } else {
        val g1 = d1.asInstanceOf[GFun]
        val g2 = d2.asInstanceOf[GFun]
        g1.p.name < g2.p.name
      }
    }.toList

    val program = Program(sorted)
    SLLTask(goal1, program)
  }
}

import NSLL._

abstract sealed class NExpr {
  def size(): Int
  def toDoc: Document
  override def toString = {
    val doc1 = toDoc
    val writer1 = new java.io.StringWriter()
    doc1.format(140, writer1)
    writer1.toString
  }
}

case class NVar(n: String) extends NExpr {
  val size = 1
  def toDoc = text(n)
}
case class NCtr(name: String, args: List[NExpr]) extends NExpr {
  lazy val size = 1 + (args map { _.size }).sum
  def toDoc = args match {
    case Nil => text(name + "()")
    case _ => group((name + "(") ::
      nest(2, ED :: args.foldRight(ED) { (x, y) => y match { case ED => x.toDoc; case _ => x.toDoc :: ", " :: y } }) :: ")" :: ED)
  }
}
case class NCall(name: String, args: List[NExpr]) extends NExpr {
  lazy val size = 1 + (args map { _.size }).sum
  def toDoc = args match {
    case Nil => text(name + "()")
    case _ => group((name + "(") ::
      nest(2, ED :: args.foldRight(ED) { (x, y) => y match { case ED => x.toDoc; case _ => x.toDoc :: ", " :: y } }) :: ")" :: ED)
  }
}
case class NCase(sel: NExpr, bs: List[(NPat, NExpr)]) extends NExpr {
  lazy val size = sel.size + (bs.map { _._2.size }).sum
  def toDoc = group(group("case" :/: sel.toDoc :/: "of {" :: ED) ::
    nest(2, bs.foldRight(ED) { (b, y) => ED :/: bToDoc(b) :: y }) :/: "}" :: ED)

}
case class NLet(name: String, f: NFun, in: NExpr) extends NExpr {
  lazy val size = 1 + f.args.size + f.term.size + in.size
  def toDoc = group("let" ::
    nest(2, group(ED :/: text(f.name + f.args.mkString("(", ", ", ")")) :: " = " :: f.term.toDoc))
    :/: "in" :: nest(2, ED :/: in.toDoc) :: ED)
}

case class NFun(name: String, args: List[NVar], term: NExpr)
case class NPat(name: String, args: List[NVar]) {
  override def toString = name + args.mkString("(", ", ", ")")
  def toDoc = text(toString)
}