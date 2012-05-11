package mrsc.pfp

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

// For now this context is used only during parsing.
// We lookup corresponding indexes of bound variables.
case class Context(l: List[String] = List()) {
  def addName(s: String): Context = Context(s :: l)
  def addNames(names: List[String]): Context = names.foldLeft(this)(_.addName(_))
  def isNameBound(s: String): Boolean = l.exists { _ == s }
  def name2index(s: String): Int = l.indexWhere { _ == s } match {
    case -1 => throw new Exception("identifier " + s + " is unbound")
    case i  => i
  }
}

// Parser is inspired by code for "Types and Programming Languages" by Pierce.
// See also https://github.com/ilya-klyuchnikov/tapl-scala for details.
case class PFPParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ";", "->", "=", "{", "}", "==>", ",", "|", "\\", "->", "[", "]", ":", ".", "<", ">")
  lexical.reserved += ("case", "of", "let", "letrec", "in", "fix")

  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }
  lazy val eof: PackratParser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }

  type Res[A] = Context => A

  lazy val bindings: PackratParser[GContext] =
    (binding *) <~ eof ^^ { bs => Map(bs: _*) }

  lazy val binding: PackratParser[(String, Term)] =
    (lcid <~ "=") ~ (term <~ ";") ^^ { case f ~ body => (f, body(Context())) }

  lazy val term: PackratParser[Res[Term]] = appTerm |
    ("\\" ~> lcid) ~ ("->" ~> term) ^^ { case v ~ t => ctx: Context => Abs(t(ctx.addName(v))) } |
    caseExpr |
    ("let" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id ~ v ~ in => ctx: Context => Let(v(ctx), in(ctx.addName(id))) } |
    ("letrec" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^
    { case f ~ body ~ in => ctx: Context => Let(Fix(Abs(body(ctx.addName(f)))), in(ctx.addName(f))) }

  lazy val caseExpr: PackratParser[Res[Term]] =
    ("case" ~> term <~ "of") ~ ("{" ~> branches <~ "}") ^^ { case sel ~ bs => ctx: Context => Case(sel(ctx), bs(ctx)) }

  lazy val appTerm: PackratParser[Res[Term]] =
    appTerm ~ aTerm ^^ { case t1 ~ t2 => ctx: Context => App(t1(ctx), t2(ctx)) } |
      "fix" ~> aTerm ^^ { t => ctx: Context => Fix(t(ctx)) } |
      aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      "<" ~> numericLit <~ ">" ^^ { id => ctx: Context => FVar(id.toInt) } |
      lcid ^^ { i => ctx: Context => if (ctx.isNameBound(i)) BVar(ctx.name2index(i)) else GVar(i) } |
      (ucid ~ ("(" ~> repsep(term, ",") <~ ")")) ^^ { case n ~ ts => ctx: Context => Ctr(n, ts.map(_(ctx))) }

  lazy val branches: PackratParser[Res[List[Branch]]] =
    rep1sep(branch, ";") ^^ { cs => ctx: Context => cs.map { c => c(ctx) } }

  lazy val branch: PackratParser[Res[Branch]] =
    ptr ~ ("->" ~> term) ^^ { case (ptr@Ptr(name, bs)) ~ t => ctx: Context => (ptr, t(ctx.addNames(bs.map(_.toString)))) }

  lazy val ptr: PackratParser[Ptr] =
    (ucid ~ ("(" ~> repsep(lcid, ",") <~ ")")) ^^ { case n ~ args => Ptr(n, args) }

  lazy val topTerm: PackratParser[Term] = term ^^ { _(Context()) }

  def inputTerm(s: String) = phrase(topTerm)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => sys.error(t.toString)
  }
  def inputBindings(s: String) = phrase(bindings)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => sys.error(t.toString)
  }
}