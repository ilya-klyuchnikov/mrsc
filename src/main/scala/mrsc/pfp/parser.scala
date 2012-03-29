package mrsc.pfp

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

// For now this context is used only during parsing.
// We lookup corresponding indexes of bound variables.
case class Context(l: List[String] = List()) {
  def addName(s: String): Context = Context(s :: l)
  def isNameBound(s: String): Boolean = l.exists { _ == s }
  def name2index(s: String): Int = l.indexWhere { _ == s } match {
    case -1 => throw new Exception("identifier " + s + " is unbound")
    case i  => i
  }
}

// Parser is inspired by code for "Types and Programming Languages" by Pierce.
case class PFPParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ";", "->", "=", "{", "}", "==>", ",", "|", "\\", "->", "[", "]", ":", ".", "<", ">")
  lexical.reserved += ("case", "of", "let", "letrec", "in", "fix")

  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLowerCase => id }
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpperCase => id }
  lazy val eof: PackratParser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }

  type Res[A] = Context => A

  lazy val bindings: PackratParser[Map[String, Term]] =
    (binding *) <~ eof ^^ { bs => Map(bs: _*) }

  lazy val binding: PackratParser[(String, Term)] =
    (lcid <~ "=") ~ (term <~ ";") ^^ { case f ~ body => (f, body(Context())) }

  lazy val term: PackratParser[Res[Term]] = appTerm |
    ("\\" ~> lcid) ~ ("->" ~> term) ^^ { case v ~ t => ctx: Context => Abs(t(ctx.addName(v))) } |
    ("case" ~> term <~ "of") ~ ("{" ~> branches <~ "}") ^^ { case sel ~ bs => ctx: Context => Case(sel(ctx), bs(ctx)) } |
    ("let" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id ~ v ~ in => ctx: Context => Let(v(ctx), in(ctx.addName(id))) } |
    ("letrec" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^
    { case f ~ body ~ in => ctx: Context => Let(Fix(Abs(body(ctx.addName(f)))), in(ctx.addName(f))) }

  lazy val appTerm: PackratParser[Res[Term]] =
    appTerm ~ pathTerm ^^ { case t1 ~ t2 => ctx: Context => App(t1(ctx), t2(ctx)) } |
      "fix" ~> pathTerm ^^ { t => ctx: Context => Fix(t(ctx)) } |
      pathTerm

  lazy val pathTerm: PackratParser[Res[Term]] =
    pathTerm ~ ("." ~> lcid) ^^ { case t1 ~ l => ctx: Context => DeCtr(t1(ctx), l) } |
      aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      "<" ~> lcid <~ ">" ^^ { id => ctx: Context => FVar(id) } |
      lcid ^^ { i => ctx: Context => if (ctx.isNameBound(i)) BVar(ctx.name2index(i)) else GVar(i) } |
      (ucid <~ "[") ~ (fields <~ "]") ^^ { case tag ~ fs => ctx: Context => Ctr(tag, fs(ctx)) }

  lazy val fields: PackratParser[Res[List[Field]]] =
    repsep(field, ",") ^^ { fs => ctx: Context => fs.map(_(ctx)) }
  lazy val field: PackratParser[Context => Field] =
    lcid ~ (":" ~> term) ^^ { case id ~ t => ctx: Context => (id, t(ctx)) }

  lazy val branches: PackratParser[Res[List[Branch]]] =
    rep1sep(branch, ";") ^^ { cs => ctx: Context => cs.map { c => c(ctx) } }
  lazy val branch: PackratParser[Res[Branch]] =
    (lcid <~ ":") ~ (ucid) ~ ("->" ~> term) ^^ { case id ~ tag ~ t => ctx: Context => (tag, t(ctx.addName(id))) }

  lazy val topTerm: PackratParser[Term] = term ^^ { _(Context()) }

  def inputTerm(s: String) = phrase(topTerm)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => error(t.toString)
  }
  def inputBindings(s: String) = phrase(bindings)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => error(t.toString)
  }
}