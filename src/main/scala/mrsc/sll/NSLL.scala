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