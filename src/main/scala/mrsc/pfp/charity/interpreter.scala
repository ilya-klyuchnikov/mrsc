package mrsc.pfp.charity

// please note that in Charity literature 
// a b c d == a (b(c(d)))
// {f, g} : X -> (Y x Z)
object CharityInterpreter {
  def eval(e: Expr): Expr = e match {
    // id, 
    case FoldApp(fold, CtrApp(n, e1)) => {
      // берем соответсвующую var-base, матчим с e1. 
      // Затем то, что типа С заменяем на соответсвующий fold 
      AbsApp(null, e1)
      null
    }
  }
}