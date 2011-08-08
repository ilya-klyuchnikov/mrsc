package mrsc.pfp.charity

// really this is a term logic
// for Charity

sealed trait VarBase
case class SingleVarBase(n: String) extends VarBase
case class PairVarBase(v1: VarBase, v2: VarBase) extends VarBase

// see Vesely, p.17 for details of semantics
// the syntax is taken from Yee 1995 - "Implementing the Charity Abstract Machine"
// v1, v2, ... vn are variable bases
// { x => e } (arg) - abstraction app
//  { c1 (v1) => t1 | ... | cn (vn) => tn } (arg) - case
// {| c1 : v1 => t1 | ... | cn : vn => tn |} (arg) - fold
// { v1 => t1 | ... | vn => tn } - map

sealed trait Expr
object Unit extends Expr
case class Pair(p1: Expr, p2: Expr) extends Expr
case class Proj1(e: Expr) extends Expr
case class Proj2(e: Expr) extends Expr
case class CtrApp(ctr: String, arg: Expr) extends Expr
// id(s) = s
case class Id

// these terms are reducible
case class AbsApp(abs: Abs, arg: Expr) extends Expr
case class CaseApp(caze: Case, arg: Expr) extends Expr
case class FoldApp(fold: Fold, arg: Expr) extends Expr
case class MapApp(map: Map, arg: Expr) extends Expr

// "abstracted" term
case class Abs(varBase: VarBase, body: Expr)
// "barbed wire"
case class Fold(varBase: VarBase)
case class Map(varBase: VarBase)
case class Case(varBase: VarBase)

//////////////
sealed trait Type
case object InitialType extends Type
case class TypeVar(v: String) extends Type
case class ProductType(p1: Type, p2: Type) extends Type

case class DataType(typeCtrs: TypeCtr, termCtrs: List[TermCtr])
case class TypeCtr(name: String, params: List[TypeVar])
case class TermCtr(name: String, argType: Type)