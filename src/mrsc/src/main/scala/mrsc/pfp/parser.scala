package mrsc.pfp

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

case class PContext(l: List[String] = List()) {
  def addName(s: String): PContext = PContext(s :: l)
  def addNames(names: List[String]): PContext = names.foldLeft(this)(_.addName(_))
  def isNameBound(s: String): Boolean = l.exists { _ == s }
  def name2index(s: String): Int = l.indexWhere { _ == s } match {
    case -1 => throw new Exception("identifier " + s + " is unbound")
    case i  => i
  }
  def index2Name(i: Int): String = l(i)
  def pickFreshName(n: String, i: Int = 0): (PContext, String) =
    if (isNameBound(n + i)) pickFreshName(n, i + 1)
    else (addName(n + i), n + i)
}

case class Task(goal: Term, bindings: GContext, name: String = "") {
  override def toString =
    (goal.toString :: (bindings.map{case (k, v) => k + " = " + v + ";"}).toList).mkString("\n")
}

case class PFPParsers() extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ";", "->", "=", "{", "}", "==>", ",", "|", "\\", "->", "[", "]", ":", ".", "<", ">")
  lexical.reserved += ("case", "of", "let", "letrec", "in")

  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }
  lazy val eof: PackratParser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }

  type Res[A] = PContext => A

  lazy val bindings: PackratParser[GContext] =
    (binding *) ^^ { bs => Map(bs: _*) }

  lazy val binding: PackratParser[(String, Term)] =
    (lcid <~ "=") ~ (term <~ ";") ^^ { case f ~ body => (f, body(PContext())) }

  lazy val term: PackratParser[Res[Term]] = appTerm |
    ("\\" ~> lcid) ~ ("->" ~> term) ^^ { case v ~ t => ctx: PContext => Abs(t(ctx.addName(v))) } |
    caseExpr |
    ("let" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id ~ v ~ in => ctx: PContext => Let(v(ctx), in(ctx.addName(id))) } |
    ("letrec" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^
    { case f ~ body ~ in => ctx: PContext => Let(Fix(body(ctx.addName(f))), in(ctx.addName(f))) }

  lazy val caseExpr: PackratParser[Res[Term]] =
    ("case" ~> term <~ "of") ~ ("{" ~> branches <~ "}") ^^ { case sel ~ bs => ctx: PContext => Case(sel(ctx), bs(ctx)) }

  lazy val appTerm: PackratParser[Res[Term]] =
    appTerm ~ aTerm ^^ { case t1 ~ t2 => ctx: PContext => App(t1(ctx), t2(ctx)) } | aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      "<" ~> numericLit <~ ">" ^^ { id => ctx: PContext => FVar(id.toInt) } |
      lcid ^^ { i => ctx: PContext => if (ctx.isNameBound(i)) BVar(ctx.name2index(i)) else GVar(i) } |
      (ucid ~ ("(" ~> repsep(term, ",") <~ ")")) ^^ { case n ~ ts => ctx: PContext => Ctr(n, ts.map(_(ctx))) }

  lazy val branches: PackratParser[Res[List[Branch]]] =
    rep1sep(branch, ";") ^^ { cs => ctx: PContext => cs.map { c => c(ctx) } }

  lazy val branch: PackratParser[Res[Branch]] =
    ptr ~ ("->" ~> term) ^^ { case (ptr @ Ptr(name, bs)) ~ t => ctx: PContext => (ptr, t(ctx.addNames(bs.map(_.toString)))) }

  lazy val ptr: PackratParser[Ptr] =
    (ucid ~ ("(" ~> repsep(lcid, ",") <~ ")")) ^^ { case n ~ args => Ptr(n, args) }

  lazy val topTerm: PackratParser[Term] = term ^^ { _(PContext()) }

  def inputTerm(s: String) = phrase(topTerm)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => sys.error(t.toString)
  }
  def inputBindings(s: String) = phrase(bindings)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => sys.error(t.toString)
  }
  lazy val task: PackratParser[Task] =
    (topTerm <~ ";") ~ bindings ^^ {case goal ~ bs => Task(goal, bs)}

  def inputTask(s: String) = phrase(task)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => sys.error(t.toString)
  }
}

// TODO
object NamedSyntax {
  def named(t: Term, ctx: PContext = PContext()): String = t match {
    case BVar(i, _)  => ctx.index2Name(i)
    case fv: FVar => "<" + fv.i.toString + ">"
    case gv: GVar => gv.n.toString
    case Abs(t1, _) =>
      val (ctx1, x) = ctx.pickFreshName("x")
      "(\\" + x + " -> " + named(t1, ctx1) + ")"
    case App(t1, t2, _) =>
      "(" + named(t1, ctx) + " " + named(t2, ctx) + ")"
    case Let(Fix(body, _), in, _) =>
      val (ctx1, f) = ctx.pickFreshName("f")
      "(letrec " + f + " = " + named(body, ctx1) + " in " + named(in, ctx1) + ")"
    case Let(v, in, _) =>
      val (ctx1, f) = ctx.pickFreshName("f")
      "(let " + f + " = " + named(v, ctx) + " in " + named(in, ctx1) + ")"
    case Ctr(n, args, _) =>
      n + args.map(named(_, ctx)).mkString("(", ", ", ")")
    case Case(sel, bs, _) =>
      "case " + named(sel, ctx) + " of " +
        (bs.map {
          case (p @ Ptr(n, args), t) =>
            val (args1, ctx2) = args.foldLeft((List[String](), ctx)) { (acc, arg) => val (ctx1, a1) = acc._2.pickFreshName(arg); (acc._1 :+ a1, ctx1) }
            Ptr(n, args1) + " -> " + named(t, ctx2)
        }).mkString("{", "; ", "}")
  }
}
