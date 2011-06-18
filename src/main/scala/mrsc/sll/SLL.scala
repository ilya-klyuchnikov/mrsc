package mrsc.sll

import mrsc._

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{CharSequenceReader => Reader}

// SLL = Simple Lazy Language, first-order with simple pattern matching.
// The intended semantics is the usual normal order reduction of the lambda calculus.

sealed trait Expr {
  def size(): Int
}

case class Var(name: String) extends Expr {
  val size = 1
  override def toString = name
}
case class Ctr(name: String, args: List[Expr]) extends Expr {
  lazy val size = 1 + (args map { _.size }).sum
  override def toString = name + args.mkString("(", ", ", ")")
}
case class FCall(name: String, args: List[Expr]) extends Expr {
  lazy val size = 1 + (args map { _.size }).sum
  override def toString = name + args.mkString("(", ", ", ")")

}
case class GCall(name: String, args: List[Expr]) extends Expr {
  lazy val size = 1 + (args map { _.size }).sum
  override def toString = name + args.mkString("(", ", ", ")")
}
case class Let(term: Expr, bindings: List[(Name, Expr)]) extends Expr {
  lazy val size = 1 + (bindings map { _._2.size }).sum
  override def toString = "let " + (bindings map { case (x, y) => x + "=" + y } mkString (", ")) + " in " + term
}
case class Where(e: Expr, defs: List[Def]) extends Expr {
  lazy val size = 1 + (defs map { _.rhs.size }).sum
  override def toString = " let " + defs.mkString("{", " ", "}") + " in " + e 
}
case class Pat(name: String, args: List[Var]) {
  override def toString = name + args.mkString("(", ", ", ")")
}

sealed abstract class Def {
  def name: String
  def lhs: Expr
  def rhs: Expr
  override def toString = lhs + " = " + rhs + ";"
}
case class FFun(name: String, args: List[Var], term: Expr) extends Def {
  override val lhs = FCall(name, args)
  override val rhs = term
}
case class GFun(name: String, p: Pat, args: List[Var], term: Expr) extends Def {
  override val lhs = GCall(name, Ctr(p.name, p.args) :: args)
  override val rhs = term
}

case class Program(defs: List[Def]) {
  val f = (defs :\ (Map[String, FFun]())) { case (x: FFun, m) => m + (x.name -> x); case (_, m) => m }
  val g = (defs :\ (Map[(String, String), GFun]())) { case (x: GFun, m) => m + ((x.name, x.p.name) -> x); case (_, m) => m }
  val gs = (defs :\ Map[String, List[GFun]]().withDefaultValue(Nil)) { case (x: GFun, m) => m + (x.name -> (x :: m(x.name))); case (_, m) => m }
  override def toString = defs.mkString("\n")
}

case class SLLTask(target: Expr, program: Program) {
  override def toString = target + "\n" + program
}

object SLLParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";")
  def prog = definition+
  def definition: Parser[Def] = gFun | fFun
  def term: Parser[Expr] = fcall | gcall | ctr | vrb
  def uid = ident ^? {case id if id.charAt(0).isUpper => id}
  def lid = ident ^? {case id if id.charAt(0).isLower => id}
  def fid = ident ^? {case id if id.charAt(0) == 'f' => id}
  def gid = ident ^? {case id if id.charAt(0) == 'g' => id}
  def vrb = lid ^^ Var
  def pat = uid ~ ("(" ~> repsep(vrb, ",") <~ ")") ^^ Pat
  def fFun = fid ~ ("(" ~> repsep(vrb, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFun
  def gFun = 
    gid ~ ("(" ~> pat) ~ ((("," ~> vrb)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFun
  def ctr = uid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ Ctr
  def fcall = fid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall
  def gcall = gid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ GCall
  def parseProg(s: String) = Program(prog(new lexical.Scanner(new Reader(s))).get)
  def parseExpr(s: String) = term(new lexical.Scanner(new Reader(s))).get
}