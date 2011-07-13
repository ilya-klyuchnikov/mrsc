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

case object MOESI extends Protocol {
  val start: Counter = List(Omega, 0, 0, 0, 0)
  val rules: List[TransitionRule] =
    List({ // rm
      case List(i, e, s, m, o) if i >= 1 =>
        List(i - 1, 0, s + e + 1, 0, o + m)
    }, { //wh2
      case List(i, e, s, m, o) if e >= 1 =>
        List(i, e - 1, s, m + 1, o)
    }, { // wh3
      case List(i, e, s, m, o) if s + o >= 1 =>
        List(i + e + s + m + o - 1, 1, 0, 0, 0)
    }, {
      case List(i, e, s, m, o) if i >= 1 =>
        List(i + e + s + m + o - 1, 1, 0, 0, 0)
    })

  def safe(c: Counter) = c match {
    case List(i, e, s, m, o) if m >= 1 && (e + s + o) >= 1 => false
    case List(i, e, s, m, o) if m >= 2 => false
    case List(i, e, s, m, o) if e >= 2 => false
    case _ => true
  }
}

case object Illinois extends Protocol {
  val start: Counter = List(Omega, 0, 0, 0)
  val rules: List[TransitionRule] =
    List({ // r2
      case List(i, e, d, s) if i >= 1 && e === 0 && d === 0 && s === 0 =>
        List(i - 1, 1, 0, 0)
    }, { // r3
      case List(i, e, d, s) if i >= 1 && d >= 1 =>
        List(i - 1, e, d - 1, s + 2)
    }, { // r4
      case List(i, e, d, s) if i >= 1 && s + e >= 1 =>
        List(i - 1, 0, d, s + e + 1)
    }, { // r6
      case List(i, e, d, s) if e >= 1 =>
        List(i, e - 1, d + 1, s)
    }, { // r7
      case List(i, e, d, s) if s >= 1 =>
        List(i + s - 1, e, d + 1, 0)
    }, { // r8
      case List(i, e, d, s) if i >= 1 =>
        List(i + e + d + s - 1, 0, 1, 0)
    }, { // r9
      case List(i, e, d, s) if d >= 1 =>
        List(i + 1, e, d - 1, s)
    }, { // r10
      case List(i, e, d, s) if s >= 1 =>
        List(i + 1, e, d, s - 1)
    }, { // r11
      case List(i, e, d, s) if e >= 1 =>
        List(i + 1, e - 1, d, s)
    })

  def safe(c: Counter) = c match {
    case List(i, e, d, s) if d >= 1 && s >= 1 => false
    case List(i, e, d, s) if d >= 2 => false
    case _ => true
  }
}

case object Berkley extends Protocol {
  val start: Counter = List(Omega, 0, 0, 0)
  val rules: List[TransitionRule] =
    List({ // rm
      case List(i, n, u, e) if i >= 1 =>
        List(i - 1, n + e, u + 1, 0)
    }, { // wm 
      case List(i, n, u, e) if i >= 1 =>
        List(i + n + u + e - 1, 0, 0, 1)
    }, { // wh1 
      case List(i, n, u, e) if n + u >= 1 =>
        List(i + n + u - 1, 0, 0, e + 1)
    })

  def safe(c: Counter) = c match {
    case List(i, n, u, e) if e >= 1 && u + n >= 1 => false
    case List(i, n, u, e) if e >= 2 => false
    case _ => true
  }
}

case object Firefly extends Protocol {
  val start: Counter = List(Omega, 0, 0, 0)
  val rules: List[TransitionRule] =
    List({ // rm1
      case List(i, e, s, d) if i >= 1 && d === 0 && s === 0 && e === 0 =>
        List(i - 1, 1, 0, 0)
    }, { // rm2
      case List(i, e, s, d) if i >= 1 && d >= 1 =>
        List(i - 1, e, s + 2, d - 1)
    }, { // rm3
      case List(i, e, s, d) if i >= 1 && s + e >= 1 =>
        List(i - 1, 0, s + e + 1, d)
    }, { // wh2
      case List(i, e, s, d) if e >= 1 =>
        List(i, e - 1, s, d + 1)
    }, { // wh3
      case List(i, e, s, d) if s === 1 =>
        List(i, e + 1, 0, d)
    }, { // wm
      case List(i, e, s, d) if i >= 1 =>
        List(i + e + d + s - 1, 0, 0, 1)
    })

  def safe(c: Counter) = c match {
    case List(i, e, s, d) if d >= 1 && s + e >= 1 => false
    case List(i, e, s, d) if e >= 2 => false
    case List(i, e, s, d) if d >= 2 => false
    case _ => true
  }
}

case object Futurebus extends Protocol {
  //val start: Counter = List(Omega, 0, 0, 0, 0, 0, 0, 0, 0)
  val start: Counter = List(ϖ, 0, 0, 0, ϖ, 0, 0, 0, ϖ)
  val rules: List[TransitionRule] =
    List({ // r2
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if i >= 1 && pW === 0 =>
        List(i - 1, 0, 0, 0, pR + 1, pW, pEMR + eM, pEMW, pSU + sU + eU)
    }, { // r3
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pEMR >= 1 =>
        List(i, sU + pR + 1, eU, eM, 0, pW, pEMR - 1, pEMW, pSU)
    }, { // r4
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pSU >= 1 =>
        List(i, sU + pR + pSU, eU, eM, 0, pW, pEMR, pEMW, 0)
    }, { // r5
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pR >= 2 && pSU === 0 && pEMR === 0 =>
        List(i, sU + pR, eU, eM, 0, pW, 0, pEMW, 0)
    }, { // r6
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pR === 1 && pSU === 0 && pEMR === 0 =>
        List(i, sU, eU + 1, eM, 0, pW, 0, pEMW, 0)
    }, { // wm1
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if i >= 1 & pW === 0 =>
        List(i + eU + sU + pSU + pR + pEMR - 1, 0, 0, 0, 0, 1, 0, pEMW + eM, 0)
    }, { // wm2
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pEMW >=1 =>
        List(i + 1, sU, eU, eM + pW, pR, 0, pEMR, pEMW - 1, pSU)
    }, { // wm3
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pEMW === 0 =>
        List(i, sU, eU, eM + pW, pR, 0, pEMR, 0, pSU)
    }, { // wh2
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if eU >= 1 =>
        List(i, sU, eU - 1, eM + 1, pR, pW, pEMR, pEMW, pSU)
    }, { // wh2
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if sU >= 1 =>
        List(i + sU - 1, 0, eU, eM + 1, pR, pW, pEMR, pEMW, pSU)
    })

  def safe(c: Counter) = c match {
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if sU >= 1 && eU + eM >= 1 => false
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if eU + eM >= 2 => false
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pR >= 1 && pW >= 1 => false
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pW >= 2 => false
    case _ => true
  }
}

//invalid ≥ 1, dirty = 0, shared_clean = 0, shared_dirty = 0, exclusive = 0 —>
case object Xerox extends Protocol {
  val start: Counter = List(Omega, 0, 0, 0, 0)
  val rules: List[TransitionRule] =
    List({
      case List(i, d, sc, sd, e) if i >= 1 && d === 0 && sc === 0 && sd === 0 && e === 0 =>
        List(i - 1, 0, 0, 0, 1)
    }, { // rm2
      case List(i, d, sc, sd, e) if i >= 1 && d + sc + e + sd >= 1 =>
        List(i - 1, 0, sc + e + 1, sd + d, 0)
    }, { // wm1
      case List(i, d, sc, sd, e) if i >= 1 && d === 0 && sc === 0 && sd === 0 && e === 0 =>
        List(i - 1, 1, 0, 0, 0)
    }, { // wm2
      case List(i, d, sc, sd, e) if i >= 1 && d + sc + e + sd >= 1 =>
        List(i - 1, 0, sc + e + 1 + sd + d, sd, 0)
    }, { // wh1
      case List(i, d, sc, sd, e) if d >= 1 =>
        List(i + 1, d - 1, sc, sd, e)
    }, { // wh2
      case List(i, d, sc, sd, e) if sc >= 1 =>
        List(i + 1, d, sc - 1, sd, e)
    }, { // wh3
      case List(i, d, sc, sd, e) if sd >= 1 =>
        List(i + 1, d, sc, sd - 1, e)
    }, { // wh4
      case List(i, d, sc, sd, e) if e >= 1 =>
        List(i + 1, d, sc, sd, e - 1)
    })

  def safe(c: Counter) = c match {
    case List(i, d, sc, sd, e) if d >= 1 && (e + sc + sd) >= 1 => false
    case List(i, d, sc, sd, e) if e >= 1 && (sc + sd) >= 1 => false
    case List(i, d, sc, sd, e) if d >= 2 => false
    case List(i, d, sc, sd, e) if e >= 2 => false
    case _ => true
  }
}