package mrsc.counters

object protocols {

  val MESI: List[TransitionRule] = List(
    {
      case List(invalid, exclusive, shared, modified) if invalid >= 1 =>
        List(invalid - 1, 0, shared + exclusive + modified + 1, 0)
    }, {
      case List(invalid, exclusive, shared, modified) if exclusive >= 1 =>
        List(invalid, exclusive - 1, shared, modified + 1)
    }, {
      case List(invalid, exclusive, shared, modified) if shared + invalid >= 1 =>
        List(invalid + exclusive + shared + modified - 1, 1, 0, 0)
    })

  def safeMESI(c: Counter) = c match {
    case List(invalid, exclusive, shared, modified) if modified >= 2 => false
    case List(invalid, exclusive, shared, modified) if shared >= 1 && modified >= 1 => false
    case _ => true
  }
}