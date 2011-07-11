package mrsc.counters

trait Protocol {
  val start: Counter
  val rules: List[TransitionRule]
  def safe(c: Counter): Boolean
}

case object Synapse extends Protocol {
  val start: Counter = List(Omega, 0, 0)
  val rules: List[TransitionRule] =
    List({
      case List(i, d, v) if i >= 1 =>
        List(i + d - 1, 0, v + 1)
    }, {
      case List(i, d, v) if v >= 1 =>
        List(i + d + v - 1, 1, 0)
    }, {
      case List(i, d, v) if i >= 1 =>
        List(i + d + v - 1, 1, 0)
    })

  def safe(c: Counter) = c match {
    case List(i, d, v) if d >= 1 && v >= 1 => false
    case List(i, d, v) if d >= 2 => false
    case _ => true
  }
}

// invalid, modified, shared
case object MSI extends Protocol {
  val start: Counter = List(Omega, 0, 0)
  val rules: List[TransitionRule] =
    List({
      case List(i, m, s) if i >= 1 =>
        List(i + m + s - 1, 1, 0)
    }, {
      case List(i, m, s) if i >= 1 =>
        List(i, m, s)
    }, {
      case List(i, m, s) if i >= 1 =>
        List(i - 1, 0, m + s + 1)
    })

  def safe(c: Counter) = c match {
    case List(i, m, s) if m >= 1 && s >= 1 => false
    case List(i, m, s) if m >= 2 => false
    case _ => true
  }
}

// invalid, modified, shared, owned
case object MOSI extends Protocol {
  val start: Counter = List(Omega, 0, 0, 0)
  val rules: List[TransitionRule] =
    List({
      case List(i, o, s, m) if i >= 1 =>
        List(i - 1, m + o, s + 1, 0)
    }, {
      case List(i, o, s, m) if o >= 1 =>
        List(i + o + s + m - 1, 0, 0, 1)
    }, {
      // wI
      case List(i, o, s, m) if i >= 1 =>
        List(i + o + s + m - 1, 0, 0, 1)
    }, {
      // wS
      case List(i, o, s, m) if s >= 1 =>
        List(i + o + s + m - 1, 0, 0, 1)
    }, {
      // se
      case List(i, o, s, m) if s >= 1 =>
        List(i + 1, o, s - 1, m)
    }, {
      // wbm
      case List(i, o, s, m) if m >= 1 =>
        List(i + 1, o, s, m - 1)
    }, {
      // wbo
      case List(i, o, s, m) if o >= 1 =>
        List(i + 1, o - 1, s, m)
    })

  def safe(c: Counter) = c match {
    case List(i, o, s, m) if o >= 2 => false
    case List(i, o, s, m) if m >= 2 => false
    case List(i, o, s, m) if s >= 1 && m >= 1 => false
    case _ => true
  }
}

case object MESI extends Protocol {
  val start: Counter = List(Omega, 0, 0, 0)
  val rules: List[TransitionRule] =
    List({
      case List(i, e, s, m) if i >= 1 =>
        List(i - 1, 0, s + e + m + 1, 0)
    }, {
      case List(i, e, s, m) if e >= 1 =>
        List(i, e - 1, s, m + 1)
    }, {
      case List(i, e, s, m) if s >= 1 =>
        List(i + e + s + m - 1, 1, 0, 0)
    }, {
      case List(i, e, s, m) if i >= 1 =>
        List(i + e + s + m - 1, 1, 0, 0)
    })

  def safe(c: Counter) = c match {
    case List(i, e, s, m) if m >= 2 => false
    case List(i, e, s, m) if s >= 1 && m >= 1 => false
    case _ => true
  }
}