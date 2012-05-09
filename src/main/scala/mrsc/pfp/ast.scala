package mrsc.pfp

// This is the core representation of 
// a simple higher-order pure functional language.
// The main purpose of this representation is to move
// to use nameless variables as much as possible
// in order to make easy normalization by super-
// compilation and two-level supercompilation.
sealed trait Term
// Nameless bound variable coded by means of de-Bruijn indices.
// De-Bruijn indices are indexed from zero.
case class BVar(i: Int) extends Term {
  override def toString = i.toString
}
// Free variables are nameless.
// It is done in order to simplify generating
// of new variables.
case class FVar(i: Int) extends Term {
  override def toString = "<" + i + ">"
}
// Global variables are named for the same reasons.
case class GVar(n: String) extends Term {
  override def toString = n
}
// Lambda abstraction.
case class Abs(t: Term) extends Term {
  override def toString = "(\\" + t + ")"
}
// Application.
case class App(t1: Term, t2: Term) extends Term {
  override def toString = "(" + t1 + " " + t2 + ")"
}
// Simple let-expression. `v` is represented by `BVar(0)` in `in`.
case class Let(v: Term, in: Term) extends Term
// Fix point combinator.
// Term itself is represented as `BVar(0)` in `t`.
case class Fix(t: Term) extends Term
// Constructor with explicit labeling of its parts.
// For example, the term `Cons x xs` will be represented as
// `Ctr("Cons", [("head", x), ("tail", y)])`
case class Ctr(tag: String, fields: List[Field]) extends Term {
  override def toString = tag + fields.map(f => f._1 + ": " + f._2).mkString("[", ", ", "]")
}
// Extracting part of a term. Usually a result of case-expression.
// Example:
// `case x of {Cons x y -> y}` will be something like:
// `case x of {... -> x.head}` = 
// `case x of {... -> DeCtr(x, head)}` 
case class DeCtr(term: Term, field: String) extends Term {
  override def toString = term + "." + field
}
// Inside the branch the selector parts matched by pattern are referenced
// by means of 
// Its parts are referenced by 0.head, 0.tail, ...
// Example:
// `case x of {Nil -> Nil; Cons y ys -> y}` = 
// `case x of {"Nil" -> Ctr("Nil", []), "Cons" -> 0.head}
case class Case(sel: Term, branches: List[Branch]) extends Term {
  override def toString = "case " + sel + " of " + branches.map(b => b._1 + "-> " + b._2).mkString("{", "; ", "}")
}