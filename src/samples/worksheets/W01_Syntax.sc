import mrsc.pfp._

import scalaz._
import Scalaz._

import NamelessShows._

// Sample showing syntax representation
object NamelessSyntaxSample {

  // free variable
  val freeVar = FVar(1)                           //> freeVar  : mrsc.pfp.FVar = FVar(1)
  // free variables is shown inside angle brackets
  freeVar.shows                                   //> res0: String = <1>

  // bound variable
  val boundVar = BVar(1)                          //> boundVar  : mrsc.pfp.BVar = BVar(1)
  boundVar.shows                                  //> res1: String = 1

  // global variable
  val globalVar = GVar("f")                       //> globalVar  : mrsc.pfp.GVar = GVar(f)
  globalVar.shows                                 //> res2: String = f

  // abstraction
  val abstraction = Abs(BVar(0))                  //> abstraction  : mrsc.pfp.Abs = Abs(BVar(0))
  abstraction.shows                               //> res3: String = (\0)

  // application
  val application = App(FVar(1), FVar(2))         //> application  : mrsc.pfp.App = App(FVar(1),FVar(2))
  application.shows                               //> res4: String = (<1> <2>)

  // constructor
  val constructor =
    Ctr("Cons", List(FVar(1), FVar(2)))           //> constructor  : mrsc.pfp.Ctr = Ctr(Cons,List(FVar(1), FVar(2)))
  constructor.shows                               //> res5: String = Cons(<1>, <2>)

  // Case-expression
  val caze = Case(FVar(1),
    (Ptr("Nil", List()) -> FVar(1)) ::
      (Ptr("Cons", List("a", "b")) -> BVar(2)) ::
      Nil)                                        //> caze  : mrsc.pfp.Case = Case(FVar(1),List((Ptr(Nil,List()),FVar(1)), (Ptr(Co
                                                  //| ns,List(a, b)),BVar(2))))
  caze.shows                                      //> res6: String = case <1> of {Nil() -> <1>; Cons(_, _) -> 2}

  val fix = Fix("f")                              //> fix  : mrsc.pfp.Fix = Fix(GVar(f))
  fix.shows                                       //> res7: String = (#f)
  
  val let = Let(FVar(1), BVar(0))                 //> let  : mrsc.pfp.Let = Let(FVar(1),BVar(0))
  let.shows                                       //> res8: String = (let <1> in 0)
  
  val rebuilding =
    Rebuilding(application,
      Map(FVar(1) -> FVar(3), FVar(2) -> FVar(3)))//> rebuilding  : mrsc.pfp.Rebuilding = Rebuilding(App(FVar(1),FVar(2)),Map(FVar
                                                  //| (1) -> FVar(3), FVar(2) -> FVar(3)))
  rebuilding.shows                                //> res9: String = (<1> <2>)/[<1> -> <3>, <2> -> <3>]
}