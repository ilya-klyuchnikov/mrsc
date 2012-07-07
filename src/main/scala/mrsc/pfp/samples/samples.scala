package mrsc.pfp.samples

import mrsc.pfp._

object samples {
  def main(args: Array[String]) {
    SREPL.sc(SC2, "rev")

    object msgGen extends MSG

    println(msgGen.termMSG("case x of {Nil(x) -> x}", "case x of {Nil(x) -> y}"))

    println(msgGen.termMSG(
      """
        case 
            <102> 
        of {Nil() -> Nil(); Cons(x1, xs) -> ((app (rev xs)) Cons(x, Nil()))}
        """,
      """
        case 
            case <104> of {Nil() -> Nil(); Cons(x1, xs) -> ((app (rev xs)) Cons(x1, Nil()))}
        of {Nil() -> Cons(<103>, Nil()); Cons(x1, xs) -> Cons(x1, ((app xs) Cons(<103>, Nil())))} 
        """))

    println(msgGen.termMSG(
      """
        case 
            case 
                <102> 
            of {Nil() -> Nil(); Cons(x1, xs) -> ((app (rev xs)) Cons(x1, Nil()))} 
        of {Nil() -> Cons(<101>, Nil()); Cons(x1, xs) -> Cons(x1, ((app xs) Cons(<101>, Nil())))}

        """,
        """
        case 
            case 
                <2>
            of {Nil() -> Cons(<103>, Nil()); Cons(x1, xs) -> Cons(x1, ((app xs) Cons(<103>, Nil())))} 
        of {Nil() -> Cons(<101>, Nil()); Cons(x1, xs) -> Cons(x1, ((app xs) Cons(<101>, Nil())))} 
        """))
  }
}