package mrsc.pfp

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

// TODO: let, letrec
object PFPParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ";", "->", "=", "{", "}", "==>", ",", "|", "\\", "->", "[", "]", ":", ".")
  lexical.reserved += ("case", "of", "let", "letrec", "in")

  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLowerCase => id }
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpperCase => id }

  // needs a context to fulfill holes
  type Res[A] = Context => A

  lazy val term: PackratParser[Res[Term]] = appTerm |
    ("\\" ~> lcid) ~ ("->" ~> term) ^^ { case v ~ t => ctx: Context => Abs(t(ctx.addName(v))) } |
    ("case" ~> term <~ "of") ~ ("{" ~> branches <~ "}") ^^ { case sel ~ bs => ctx: Context => Case(sel(ctx), bs(ctx)) }

  lazy val appTerm: PackratParser[Res[Term]] =
    appTerm ~ pathTerm ^^ { case t1 ~ t2 => ctx: Context => App(t1(ctx), t2(ctx)) } |
      pathTerm

  lazy val pathTerm: PackratParser[Res[Term]] =
    pathTerm ~ ("." ~> lcid) ^^ { case t1 ~ l => ctx: Context => DeCtr(t1(ctx), l) } |
      aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
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
}