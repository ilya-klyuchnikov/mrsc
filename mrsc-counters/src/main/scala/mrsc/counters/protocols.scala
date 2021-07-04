package mrsc.counters

case object Synapse extends Protocol {
  val start: Conf = List(Omega, 0, 0)
  val rules: List[TransitionRule] =
    List(
      {
        case List(i, d, v) if i >= 1 =>
          List(i + d - 1, 0, v + 1)
      },
      {
        case List(i, d, v) if v >= 1 =>
          List(i + d + v - 1, 1, 0)
      },
      {
        case List(i, d, v) if i >= 1 =>
          List(i + d + v - 1, 1, 0)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(i, d, v) if d >= 1 && v >= 1 => true
    case List(i, d, v) if d >= 2           => true
    case _                                 => false
  }

  override val name = "synapse"

  override val isabelleEncoding: String =
    """
    |inductive synapse :: "(nat * nat * nat) => bool" where
    |  "synapse (i, 0, 0)" |
    |  "synapse (Suc i, d, v) ==> synapse (i + d, 0, Suc v)" |
    |  "synapse (i, d, Suc v) ==> synapse (i + d + v, (Suc 0), 0)" |
    |  "synapse (Suc i, d, v) ==> synapse (i + d + v, (Suc 0), 0)"
    |
    |inductive unsafe :: "(nat * nat * nat) => bool" where 
    |  "unsafe (x, Suc y, Suc z)" |
    |  "unsafe (x, Suc (Suc y), z)"
    """.stripMargin

}

// invalid, modified, shared
case object MSI extends Protocol {
  val start: Conf = List(Omega, 0, 0)
  val rules: List[TransitionRule] =
    List(
      {
        case List(i, m, s) if i >= 1 =>
          List(i + m + s - 1, 1, 0)
      },
      {
        case List(i, m, s) if s >= 1 =>
          List(i + m + s - 1, 1, 0)
      },
      {
        case List(i, m, s) if i >= 1 =>
          List(i - 1, 0, m + s + 1)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(i, m, s) if m >= 1 && s >= 1 => true
    case List(i, m, s) if m >= 2           => true
    case _                                 => false
  }

  override val name = "msi"

  override val isabelleEncoding: String =
    """
    |inductive msi :: "(nat * nat * nat) => bool" where
    |  "msi (i, 0, 0)" |
    |  "msi (Suc i, m, s) ==> msi (i + m + s, Suc 0, 0)" |
    |  "msi (i, m, Suc s) ==> msi (i + m + s, Suc 0, 0)" |
    |  "msi (Suc i, m, s) ==> msi (i, 0, Suc (m + s))"
    |
    |inductive unsafe :: "(nat * nat * nat) => bool" where 
    |  "unsafe (x, Suc y, Suc z)" |
    |  "unsafe (x, Suc (Suc y), z)"
    """.stripMargin

}

// invalid, modified, shared, owned
case object MOSI extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0)
  val rules: List[TransitionRule] =
    List(
      {
        case List(i, o, s, m) if i >= 1 =>
          List(i - 1, m + o, s + 1, 0)
      },
      {
        case List(i, o, s, m) if o >= 1 =>
          List(i + o + s + m - 1, 0, 0, 1)
      },
      { // wI
        case List(i, o, s, m) if i >= 1 =>
          List(i + o + s + m - 1, 0, 0, 1)
      },
      { // wS
        case List(i, o, s, m) if s >= 1 =>
          List(i + o + s + m - 1, 0, 0, 1)
      },
      { // se
        case List(i, o, s, m) if s >= 1 =>
          List(i + 1, o, s - 1, m)
      },
      { // wbm
        case List(i, o, s, m) if m >= 1 =>
          List(i + 1, o, s, m - 1)
      },
      { // wbo
        case List(i, o, s, m) if o >= 1 =>
          List(i + 1, o - 1, s, m)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(i, o, s, m) if o >= 2           => true
    case List(i, o, s, m) if m >= 2           => true
    case List(i, o, s, m) if s >= 1 && m >= 1 => true
    case _                                    => false
  }

  override val name = "mosi"

  // using o as identifier in Isabelle produces errors (why?)
  // so  we use o' here
  override val isabelleEncoding: String =
    """
    |inductive mosi :: "(nat * nat * nat * nat) => bool" where
    |  "mosi (i, 0, 0, 0)" |
    |  "mosi (Suc i, o', s, m) ==> mosi (i, m + o', Suc s, 0)" |
    |  "mosi (i, Suc o', s, m) ==> mosi (i + o' + s + m, 0, 0, Suc 0)" |
    |  "mosi (Suc i, o', s, m) ==> mosi (i + o' + s + m, 0, 0, Suc 0)" |
    |  "mosi (i, o', Suc s, m) ==> mosi (i + o' + s + m, 0, 0, Suc 0)" |
    |  "mosi (i, o', Suc s, m) ==> mosi (Suc i, o', s, m)" |
    |  "mosi (i, o', s, Suc m) ==> mosi (Suc i, o', s, m)" |
    |  "mosi (i, Suc o', s, m) ==> mosi (Suc i, o', s, m)"
    |
    |inductive unsafe :: "(nat * nat * nat * nat) => bool" where 
    |  "unsafe (i, Suc (Suc o'), s, m)" |
    |  "unsafe (i, o', s, Suc (Suc m))" |
    |  "unsafe (i, o', Suc s, Suc m)" 
    """.stripMargin
}

case object MESI extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0)
  val rules: List[TransitionRule] =
    List(
      {
        case List(i, e, s, m) if i >= 1 =>
          List(i - 1, 0, s + e + m + 1, 0)
      },
      {
        case List(i, e, s, m) if e >= 1 =>
          List(i, e - 1, s, m + 1)
      },
      {
        case List(i, e, s, m) if s >= 1 =>
          List(i + e + s + m - 1, 1, 0, 0)
      },
      {
        case List(i, e, s, m) if i >= 1 =>
          List(i + e + s + m - 1, 1, 0, 0)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(i, e, s, m) if m >= 2           => true
    case List(i, e, s, m) if s >= 1 && m >= 1 => true
    case _                                    => false
  }

  override val name = "mesi"

  override val isabelleEncoding: String =
    """
    |inductive mesi :: "(nat * nat * nat * nat) => bool" where
    |  "mesi (i, 0, 0, 0)" |
    |  "mesi (Suc i, e, s, m) ==> mesi (i, 0, Suc (s + e + m), 0)" |
    |  "mesi (i, Suc e, s, m) ==> mesi (i, e, s, Suc m)" |
    |  "mesi (i, e, Suc s, m) ==> mesi (i + e + s + m, Suc 0, 0, 0)" |
    |  "mesi (Suc i, e, s, m) ==> mesi (i + e + s + m, Suc 0, 0, 0)"
    |
    |inductive unsafe :: "(nat * nat * nat * nat) => bool" where 
    |  "unsafe (i, e, s, Suc (Suc m))" |
    |  "unsafe (i, e, Suc s, Suc m)" 
    """.stripMargin
}

case object MOESI extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0, 0)
  val rules: List[TransitionRule] =
    List(
      { // rm
        case List(i, m, s, e, o) if i >= 1 =>
          List(i - 1, 0, s + e + 1, 0, o + m)
      },
      { //wh2
        case List(i, m, s, e, o) if e >= 1 =>
          List(i, m + 1, s, e - 1, o)
      },
      { // wh3
        case List(i, m, s, e, o) if s + o >= 1 =>
          List(i + m + s + e + o - 1, 0, 0, 1, 0)
      },
      { // wm
        case List(i, m, s, e, o) if i >= 1 =>
          List(i + m + s + e + o - 1, 0, 0, 1, 0)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(i, m, s, e, o) if m >= 1 && (e + s + o) >= 1 => true
    case List(i, m, s, e, o) if m >= 2                     => true
    case List(i, m, s, e, o) if e >= 2                     => true
    case _                                                 => false
  }

  override val name = "moesi"

  override val isabelleEncoding: String =
    """
    |inductive moesi :: "(nat * nat * nat * nat * nat) => bool" where
    |  "moesi (i, 0, 0, 0, 0)" |
    |  "moesi (Suc i, m, s, e, o') ==> moesi (i, 0, Suc (s + e), 0, m + o')" |
    |  "moesi (i, m, s, Suc e, o') ==> moesi (i, Suc m, s, e, o')" |
    |  "moesi (i, m, Suc s, e, o') ==> moesi (i + m + s + e + o', 0, 0, Suc 0, 0)" |
    |  "moesi (Suc i, m, s, e, o') ==> moesi (i + m + s + e + o', 0, 0, Suc 0, 0)"
    |
    |inductive unsafe :: "(nat * nat * nat * nat * nat) => bool" where 
    |  "unsafe (i, Suc m, Suc s, e, o')" |
    |  "unsafe (i, Suc m, s, Suc e, o')" |
    |  "unsafe (i, Suc m, s, e, Suc o')" |
    |  "unsafe (i, Suc (Suc m), s, e, o')" |
    |  "unsafe (i, m, s, Suc (Suc e), o')"
    """.stripMargin
}

case object Illinois extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0)
  val rules: List[TransitionRule] =
    List(
      { // r2
        case List(i, e, d, s) if i >= 1 && e === 0 && d === 0 && s === 0 =>
          List(i - 1, 1, 0, 0)
      },
      { // r3
        case List(i, e, d, s) if i >= 1 && d >= 1 =>
          List(i - 1, e, d - 1, s + 2)
      },
      { // r4
        case List(i, e, d, s) if i >= 1 && s + e >= 1 =>
          List(i - 1, 0, d, s + e + 1)
      },
      { // r6
        case List(i, e, d, s) if e >= 1 =>
          List(i, e - 1, d + 1, s)
      },
      { // r7
        case List(i, e, d, s) if s >= 1 =>
          List(i + s - 1, e, d + 1, 0)
      },
      { // r8
        case List(i, e, d, s) if i >= 1 =>
          List(i + e + d + s - 1, 0, 1, 0)
      },
      { // r9
        case List(i, e, d, s) if d >= 1 =>
          List(i + 1, e, d - 1, s)
      },
      { // r10
        case List(i, e, d, s) if s >= 1 =>
          List(i + 1, e, d, s - 1)
      },
      { // r11
        case List(i, e, d, s) if e >= 1 =>
          List(i + 1, e - 1, d, s)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(i, e, d, s) if d >= 1 && s >= 1 => true
    case List(i, e, d, s) if d >= 2           => true
    case _                                    => false
  }

  override val name = "illinois"

  override val isabelleEncoding: String =
    """
    |inductive illinois :: "(nat * nat * nat * nat) => bool" where
    |  start: "illinois (i, 0, 0, 0)" |
    |  r2: "illinois (Suc i, 0, 0, 0) ==> illinois (i, Suc 0, 0, 0)" |
    |  r3: "illinois (Suc i, e, Suc d, s) ==> illinois (i, e, d, Suc (Suc s))" |
    |  r4a: "illinois (Suc i, Suc e, d, s) ==> illinois (i, 0, d, Suc (s + e))" |
    |  r4b: "illinois (Suc i, e, d, Suc s) ==> illinois (i, 0, d, Suc (s + e))" |
    |  r6: "illinois (i, Suc e, d, s) ==> illinois (i, e, Suc d, s)" |
    |  r7: "illinois (i, e, d, Suc s) ==> illinois (i + s, e, Suc d, 0)" |
    |  r8: "illinois (Suc i, e, d, s) ==> illinois (i + e + d + s, 0, Suc 0, 0)" |
    |  r9: "illinois (i, e, Suc d, s) ==> illinois (Suc i, e, d, s)" |
    |  r10: "illinois (i, e, d, Suc s) ==> illinois (Suc i, e, d, s)" |
    |  r11: "illinois (i, Suc e, d, s) ==> illinois (Suc i, e, d, s)" 
    |
    |inductive unsafe :: "(nat * nat * nat * nat) => bool" where 
    |  "unsafe (i, e, Suc d, Suc s)" |
    |  "unsafe (i, e, Suc (Suc d), s)"
    """.stripMargin
}

case object Berkley extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0)
  val rules: List[TransitionRule] =
    List(
      { // rm
        case List(i, n, u, e) if i >= 1 =>
          List(i - 1, n + e, u + 1, 0)
      },
      { // wm
        case List(i, n, u, e) if i >= 1 =>
          List(i + n + u + e - 1, 0, 0, 1)
      },
      { // wh1
        case List(i, n, u, e) if n + u >= 1 =>
          List(i + n + u - 1, 0, 0, e + 1)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(i, n, u, e) if e >= 1 && u + n >= 1 => true
    case List(i, n, u, e) if e >= 2               => true
    case _                                        => false
  }

  override val name = "berkley"

  override val isabelleEncoding: String =
    """
    |inductive berkley :: "(nat * nat * nat * nat) => bool" where
    |  start: "berkley (i, 0, 0, 0)" |
    |  rm: "berkley (Suc i, n, u, e) ==> berkley (i, n + e, Suc u, 0)" |
    |  wm: "berkley (Suc i, n, u, e) ==> berkley (i + n + u + e, 0, 0, Suc 0)" |
    |  wh1: "berkley (i, Suc n, u, e) ==> berkley (i + n + u, 0, 0, Suc e)" |
    |  wh2: "berkley (i, n, Suc u, e) ==> berkley (i + n + u, 0, 0, Suc e)"
    |
    |inductive unsafe :: "(nat * nat * nat * nat) => bool" where 
    |  "unsafe (i, Suc n, u, Suc e)" |
    |  "unsafe (i, n, Suc u, Suc e)" |
    |  "unsafe (i, n, u, Suc (Suc e))"
    """.stripMargin
}

case object Firefly extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0)
  val rules: List[TransitionRule] =
    List(
      { // rm1
        case List(i, e, s, d) if i >= 1 && d === 0 && s === 0 && e === 0 =>
          List(i - 1, 1, 0, 0)
      },
      { // rm2
        case List(i, e, s, d) if i >= 1 && d >= 1 =>
          List(i - 1, e, s + 2, d - 1)
      },
      { // rm3
        case List(i, e, s, d) if i >= 1 && s + e >= 1 =>
          List(i - 1, 0, s + e + 1, d)
      },
      { // wh2
        case List(i, e, s, d) if e >= 1 =>
          List(i, e - 1, s, d + 1)
      },
      { // wh3
        case List(i, e, s, d) if s === 1 =>
          List(i, e + 1, 0, d)
      },
      { // wm
        case List(i, e, s, d) if i >= 1 =>
          List(i + e + d + s - 1, 0, 0, 1)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(i, e, s, d) if d >= 1 && s + e >= 1 => true
    case List(i, e, s, d) if e >= 2               => true
    case List(i, e, s, d) if d >= 2               => true
    case _                                        => false
  }

  override val name = "firefly"

  override val isabelleEncoding: String =
    """
    |inductive firefly :: "(nat * nat * nat * nat) => bool" where
    |  start: "firefly (i, 0, 0, 0)" |
    |  rm1:  "firefly (Suc i, 0, 0, 0) ==> firefly (i, Suc 0, 0, 0)" |
    |  rm2:  "firefly (Suc i, e, s, Suc d) ==> firefly (i, e, Suc (Suc s), d)" |
    |  rm31: "firefly (Suc i, Suc e, s, d) ==> firefly (i, 0, Suc (e + s), d)" |
    |  rm32: "firefly (Suc i, e, Suc s, d) ==> firefly (i, 0, Suc (e + s), d)" |
    |  wh2:  "firefly (i, Suc e, s, d) ==> firefly (i, e, s, Suc d)" |
    |  wh3:  "firefly (i, e, Suc s, d) ==> firefly (i, Suc e, 0, d)" |
    |  wm:   "firefly (Suc i, e, s, d) ==> firefly (i + e + s + d, 0, 0, Suc 0)" 
    |
    |inductive unsafe :: "(nat * nat * nat * nat) => bool" where 
    |  "unsafe (i, Suc e, s, Suc d)" |
    |  "unsafe (i, e, Suc s, Suc d)" |
    |  "unsafe (i, Suc (Suc e), s, d)" |
    |  "unsafe (i, e, s, Suc (Suc d))"
    """.stripMargin

}

case object Futurebus extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0, 0, 0, 0, 0, 0)
  val rules: List[TransitionRule] =
    List(
      { // r2
        case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if i >= 1 && pW === 0 =>
          List(i - 1, 0, 0, 0, pR + 1, pW, pEMR + eM, pEMW, pSU + sU + eU)
      },
      { // r3
        case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pEMR >= 1 =>
          List(i, sU + pR + 1, eU, eM, 0, pW, pEMR - 1, pEMW, pSU)
      },
      { // r4
        case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pSU >= 1 =>
          List(i, sU + pR + pSU, eU, eM, 0, pW, pEMR, pEMW, 0)
      },
      { // r5
        case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pR >= 2 && pSU === 0 && pEMR === 0 =>
          List(i, sU + pR, eU, eM, 0, pW, 0, pEMW, 0)
      },
      { // r6
        case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pR === 1 && pSU === 0 && pEMR === 0 =>
          List(i, sU, eU + 1, eM, 0, pW, 0, pEMW, 0)
      },
      { // wm1
        case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if i >= 1 & pW === 0 =>
          List(i + eU + sU + pSU + pR + pEMR - 1, 0, 0, 0, 0, 1, 0, pEMW + eM, 0)
      },
      { // wm2
        case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pEMW >= 1 =>
          List(i + 1, sU, eU, eM + pW, pR, 0, pEMR, pEMW - 1, pSU)
      },
      { // wm3
        case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pEMW === 0 =>
          List(i, sU, eU, eM + pW, pR, 0, pEMR, 0, pSU)
      },
      { // wh2
        case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if eU >= 1 =>
          List(i, sU, eU - 1, eM + 1, pR, pW, pEMR, pEMW, pSU)
      },
      { // wh2
        case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if sU >= 1 =>
          List(i + sU - 1, 0, eU, eM + 1, pR, pW, pEMR, pEMW, pSU)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if sU >= 1 && eU + eM >= 1 => true
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if eU + eM >= 2            => true
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pR >= 1 && pW >= 1      => true
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pW >= 2                 => true
    case _                                                                       => false
  }

  override val name = "futurebus"

  override val isabelleEncoding: String =
    """
    |inductive futurebus :: "(nat * nat * nat * nat * nat * nat * nat * nat * nat) => bool" where
    |  start: "futurebus (i, 0, 0, 0, 0, 0, 0, 0, 0)" |
    |  r2:   "futurebus (Suc i, sU, eU, eM, pR, 0, pEMR, pEMW, pSU) ==> futurebus (i, 0, 0, 0, Suc pR, 0, pEMR + eM, pEMW, pSU + sU + eU)" |
    |  r3:   "futurebus (i, sU, eU, eM, pR, pW, Suc pEMR, pEMW, pSU) ==> futurebus (i, Suc (sU + pR), eU, eM, 0, pW, pEMR, pEMW, pSU)" |
    |  r4:   "futurebus (i, sU, eU, eM, pR, pW, pEMR, pEMW, Suc pSU) ==> futurebus (i, Suc (sU + pR + pSU), eU, eM, 0, pW, pEMR, pEMW, 0)" |
    |  r5:   "futurebus (i, sU, eU, eM, Suc (Suc pR), pW, 0, pEMW, 0) ==> futurebus (i, Suc (Suc (sU + pR)), eU, eM, 0, pW, 0, pEMW, 0)" |
    |  r6:   "futurebus (i, sU, eU, eM, Suc (Suc 0), pW, 0, pEMW, 0) ==> futurebus (i, sU, Suc eU, eM, 0, pW, 0, pEMW, 0)" |
    |  wm1:  "futurebus (Suc i, sU, eU, eM, pR, 0, pEMR, pEMW, pSU) ==> futurebus (i + eU + sU + pSU + pR + pEMR, 0, 0, 0, 0, Suc 0, 0, pEMW + eM, 0)" |
    |  wm2:  "futurebus (i, sU, eU, eM, pR, pW, pEMR, Suc pEMW, pSU) ==> futurebus (Suc i, sU, eU, eM + pW, pR, 0, pEMR, pEMW, pSU)" |
    |  wm3:  "futurebus (i, sU, eU, eM, pR, pW, pEMR, 0, pSU) ==> futurebus (i, sU, eU, eM + pW, pR, 0, pEMR, 0, pSU)" |
    |  wh2:  "futurebus (i, sU, Suc eU, eM, pR, pW, pEMR, pEMW, pSU) ==> futurebus (i, sU, eU, Suc eM, pR, pW, pEMR, pEMW, pSU)" |
    |  wh3:  "futurebus (i, Suc sU, eU, eM, pR, pW, pEMR, pEMW, pSU) ==> futurebus (i + sU, 0, eU, Suc eM, pR, pW, pEMR, pEMW, pSU)"
    |
    |inductive unsafe :: "(nat * nat * nat * nat * nat * nat * nat * nat * nat) => bool" where 
    |  "unsafe (i, Suc sU, Suc eU, eM, pR, pW, pEMR, pEMW, pSU)" |
    |  "unsafe (i, Suc sU, eU, Suc eM, pR, pW, pEMR, pEMW, pSU)" |
    |  "unsafe (i, sU, Suc (Suc eU), eM, pR, pW, pEMR, pEMW, pSU)" |
    |  "unsafe (i, sU, eU, Suc (Suc eM), pR, pW, pEMR, pEMW, pSU)" |
    |  "unsafe (i, sU, Suc eU, Suc eM, pR, pW, pEMR, pEMW, pSU)" |
    |  "unsafe (i, sU, eU, eM, Suc pR, Suc pW, pEMR, pEMW, pSU)" |
    |  "unsafe (i, sU, eU, eM, pR, Suc (Suc pW), pEMR, pEMW, pSU)"
    |
    """.stripMargin
}

//invalid ≥ 1, dirty = 0, shared_clean = 0, shared_dirty = 0, exclusive = 0 —>
case object Xerox extends Protocol {
  val start: Conf = List(Omega, 0, 0, 0, 0)
  val rules: List[TransitionRule] =
    List(
      { // (1) rm1
        case List(i, sc, sd, d, e) if i >= 1 && d === 0 && sc === 0 && sd === 0 && e === 0 =>
          List(i - 1, 0, 0, 0, 1)
      },
      { // (2) rm2
        case List(i, sc, sd, d, e) if i >= 1 && d + sc + e + sd >= 1 =>
          List(i - 1, sc + e + 1, sd + d, 0, 0)
      },
      { // (3) wm1
        case List(i, sc, sd, d, e) if i >= 1 && d === 0 && sc === 0 && sd === 0 && e === 0 =>
          List(i - 1, 0, 0, 1, 0)
      },
      { // (4) wm2
        case List(i, sc, sd, d, e) if i >= 1 && d + sc + e + sd >= 1 =>
          List(i - 1, sc + e + 1 + sd + d, sd, 0, 0)
      },
      { // (5) wh1
        case List(i, sc, sd, d, e) if d >= 1 =>
          List(i + 1, sc, sd, d - 1, e)
      },
      { // (6) wh2
        case List(i, sc, sd, d, e) if sc >= 1 =>
          List(i + 1, sc - 1, sd, d, e)
      },
      { // (7) wh3
        case List(i, sc, sd, d, e) if sd >= 1 =>
          List(i + 1, sc, sd - 1, d, e)
      },
      { // (8) wh4
        case List(i, sc, sd, d, e) if e >= 1 =>
          List(i + 1, sc, sd, d, e - 1)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(i, sc, sd, d, e) if d >= 1 && (e + sc + sd) >= 1 => true
    case List(i, sc, sd, d, e) if e >= 1 && (sc + sd) >= 1     => true
    case List(i, sc, sd, d, e) if d >= 2                       => true
    case List(i, sc, sd, d, e) if e >= 2                       => true
    case _                                                     => false
  }

  override val name = "xerox"

  override val isabelleEncoding: String =
    """
    |inductive xerox :: "(nat * nat * nat * nat * nat) => bool" where
    |  start: "xerox (i, 0, 0, 0, 0)" |
    |  rm1:  "xerox (Suc i, 0, 0, 0, 0) ==> xerox (i, 0, 0, 0, Suc 0)" |
    |  rm2a: "xerox (Suc i, Suc sc, sd, d, e) ==> xerox (i, Suc (sc + e), sd + d, 0, 0)" |
    |  rm2b: "xerox (Suc i, sc, Suc sd, d, e) ==> xerox (i, Suc (sc + e), sd + d, 0, 0)" |
    |  rm2c: "xerox (Suc i, sc, sd, Suc d, e) ==> xerox (i, Suc (sc + e), sd + d, 0, 0)" |
    |  rm2d: "xerox (Suc i, sc, sd, d, Suc e) ==> xerox (i, Suc (sc + e), sd + d, 0, 0)" |
    |  wm1:  "xerox (Suc i, 0, 0, 0, 0) ==> xerox (i, 0, 0, Suc 0, 0)" |
    |  wm2a: "xerox (Suc i, Suc sc, sd, d, e) ==> xerox (i, Suc (sc + d + sd), sd, 0, 0)" |
    |  wm2b: "xerox (Suc i, sc, Suc sd, d, e) ==> xerox (i, Suc (sc + d + sd), sd, 0, 0)" |
    |  wm2c: "xerox (Suc i, sc, sd, Suc d, e) ==> xerox (i, Suc (sc + d + sd), sd, 0, 0)" |
    |  wm2d: "xerox (Suc i, sc, sd, d, Suc e) ==> xerox (i, Suc (sc + d + sd), sd, 0, 0)" |
    |  wh1:  "xerox (i, sc, sd, Suc d, e) ==> xerox (Suc i, sc, sd, d, e)" |
    |  wh2:  "xerox (i, Suc sc, sd, d, e) ==> xerox (Suc i, sc, sd, d, e)" |
    |  wh3:  "xerox (i, sc, Suc sd, d, e) ==> xerox (Suc i, sc, sd, d, e)" |
    |  wh4:  "xerox (i, sc, sd, d, Suc e) ==> xerox (Suc i, sc, sd, d, e)"
    |
    |inductive unsafe :: "(nat * nat * nat * nat * nat) => bool" where 
    |  "unsafe (i, Suc sc, sd, Suc d, e)" |
    |  "unsafe (i, sc, Suc sd, Suc d, e)" |
    |  "unsafe (i, sc, sd, Suc d, Suc e)" |
    |  "unsafe (i, Suc sc, sd, d, Suc e)" |
    |  "unsafe (i, sc, Suc sd, d, Suc e)" |
    |  "unsafe (i, sc, sd, Suc (Suc d), e)" |
    |  "unsafe (i, sc, sd, d, Suc (Suc e))" 
    """.stripMargin
}

case object Java extends Protocol {
  // nb 1 = True, nb 0 = False
  // race = -1 = H0, -2 = H1 ,,,
  val start: Conf = List(1, -1, Omega, 0, 0, 0, 0, 0)
  val rules: List[TransitionRule] =
    List(
      { // (get fast)
        case List(nb, race, i, b, o, in, out, w) if nb === 1 && i >= 1 =>
          List(0, race, i - 1, 0, o + 1, in, out, w)
      },
      { // (put fast)
        case List(nb, race, i, b, o, in, out, w) if nb === 0 && b === 0 && o >= 1 =>
          List(1, race, i + 1, b, o - 1, in, out, w)
      },
      { // (get slow)
        case List(nb, race, i, b, o, in, out, w) if nb === 0 && i >= 1 =>
          List(nb, race, i - 1, b + 1, o, in + 1, out, w)
      },
      { // (put slow)
        case List(nb, race, i, b, o, in, out, w) if nb === 0 && b >= 1 && o >= 1 =>
          List(nb, race, i, b - 1, o - 1, in, out + 1, w)
      },
      { // (request)
        case List(nb, race, i, b, o, in, out, w) if race === -1 && in >= 1 =>
          List(nb, -2, i, b, o, in - 1, out, w + 1)
      },
      { // (request)
        case List(nb, race, i, b, o, in, out, w) if race === -3 && in >= 1 =>
          List(nb, -4, i, b, o, in - 1, out, w + 1)
      },
      { // (release)
        case List(nb, race, i, b, o, in, out, w) if race === -1 && out >= 1 =>
          List(nb, -3, i + 1, b, o, in, out - 1, w)
      },
      { // (release)
        case List(nb, race, i, b, o, in, out, w) if race === -2 && out >= 1 =>
          List(nb, -4, i + 1, b, o, in, out - 1, w)
      },
      { // (go)
        case List(nb, race, i, b, o, in, out, w) if race === -4 && w >= 1 =>
          List(nb, race, i, b, o + 1, in, out, w - 1)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(nb, race, i, b, o, in, out, w) if o + out >= 2 => true
    case _                                                   => false
  }

  override val name = "java"

  override val isabelleEncoding: String =
    """
    |inductive java :: "(nat * nat * nat * nat * nat * nat * nat * nat) => bool" where
    |  start:  "java (Suc 0, Suc 0, i, 0, 0, 0, 0, 0)" |
    |  getf:  "java (Suc 0, race, Suc i, b, o', in', out, w) ==> java (0, race, i, 0, Suc o', in', out, w)" |
    |  putf:  "java (0, race, i, 0, Suc o', in', out, w) ==> java (Suc 0, race, Suc i, 0, o', in', out, w)" |
    |  gets:  "java (0, race, Suc i, b, o', in', out, w) ==> java (0, race, i, Suc b, o', Suc in', out, w)" |
    |  puts:  "java (0, race, i, Suc b, Suc o', in', out, w) ==> java (0, race, i, b, o', in', Suc out, w)" |
    |  req1:  "java (nb, Suc 0, i, b, o', Suc in', out, w) ==> java (nb, Suc (Suc 0), i, b, o', in', out, Suc w)" |
    |  req2:  "java (nb, Suc (Suc (Suc 0)), i, b, o', Suc in', out, w) ==> java (nb, Suc (Suc (Suc (Suc 0))), i, b, o', in', out, Suc w)" |
    |  rel1:  "java (nb, Suc 0, i, b, o', in', Suc out, w) ==> java (nb, Suc (Suc (Suc 0)), Suc i, b, o', in', out, w)" |
    |  rel2:  "java (nb, Suc (Suc 0), i, b, o', in', Suc out, w) ==> java (nb, Suc (Suc (Suc (Suc 0))), Suc i, b, o', in', out, w)" |
    |  go:  "java (nb, Suc (Suc (Suc (Suc 0))), i, b, o', in', out, Suc w) ==> java (nb, Suc (Suc (Suc (Suc 0))), Suc i, b, Suc o', in', out, w)"
    |
    |inductive unsafe :: "(nat * nat * nat * nat * nat * nat * nat * nat) => bool" where 
    |  "unsafe (nb, race, i, b, Suc (Suc o'), in', out, w)" |
    |  "unsafe (nb, race, i, b, Suc o', in', Suc out, w)" |
    |  "unsafe (nb, race, i, b, o', in', Suc (Suc out), w)"
    """.stripMargin
}

case object ReaderWriter extends Protocol {
  val start: Conf = List(1, 0, 0, Omega, 0, 0)
  val rules: List[TransitionRule] =
    List(
      { // r1
        case List(x2, x3, x4, x5, x6, x7) if x2 >= 1 && x4 === 0 && x7 >= 1 =>
          List(x2 - 1, x3 + 1, 0, x5, x6, x7)
      },
      { // r2
        case List(x2, x3, x4, x5, x6, x7) if x2 >= 1 && x6 >= 1 =>
          List(x2, x3, x4 + 1, x5, x6 - 1, x7)
      },
      { // r3
        case List(x2, x3, x4, x5, x6, x7) if x3 >= 1 =>
          List(x2 + 1, x3 - 1, x4, x5 + 1, x6, x7)
      },
      { // r4
        case List(x2, x3, x4, x5, x6, x7) if x4 >= 1 =>
          List(x2, x3, x4 - 1, x5 + 1, x6, x7)
      },
      { // r5
        case List(x2, x3, x4, x5, x6, x7) if x5 >= 1 =>
          List(x2, x3, x4, x5 - 1, x6 + 1, x7)
      },
      { // r6
        case List(x2, x3, x4, x5, x6, x7) if x5 >= 1 =>
          List(x2, x3, x4, x5 - 1, x6, x7 + 1)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(x2, x3, x4, x5, x6, x7) if x3 >= 1 && x4 >= 1 => true
    case _                                                  => false
  }

  override val name = "rw"

  override val isabelleEncoding: String =
    """
    |inductive rw :: "(nat * nat * nat * nat * nat * nat) => bool" where
    |  start:  "rw (Suc 0, 0, 0, x5, 0, 0)" |
    |  r1: "rw (Suc x2, x3, 0, x5, x6, Suc x7) ==> rw (x2, Suc x3, 0, x5, x6, Suc x7)" |
    |  r2: "rw (Suc x2, x3, x4, x5, Suc x6, x7) ==> rw (Suc x2, x3, Suc x4, x5, x6, x7)" |
    |  r3: "rw (x2, Suc x3, x4, x5, x6, x7) ==> rw (Suc x2, x3, x4, Suc x5, x6, x7)" |
    |  r4: "rw (x2, x3, Suc x4, x5, x6, x7) ==> rw (x2, x3, x4, Suc x5, x6, x7)" |
    |  r5: "rw (x2, x3, x4, Suc x5, x6, x7) ==> rw (x2, x3, x4, x5, Suc x6, x7)" |
    |  r6: "rw (x2, x3, x4, Suc x5, x6, x7) ==> rw (x2, x3, x4, x5, x6, Suc x7)" 
    |
    |inductive unsafe :: "(nat * nat * nat * nat * nat * nat) => bool" where 
    |  "unsafe (x2, Suc x3, Suc x4, x5, x6, x7)"
    """.stripMargin
}

case object DataRace extends Protocol {
  val start: Conf = List(Omega, 0, 0)
  val rules: List[TransitionRule] =
    List(
      { // 1
        case List(out, cs, scs) if out >= 1 && cs === 0 && scs === 0 =>
          List(out - 1, 1, 0)
      },
      { // 2
        case List(out, cs, scs) if out >= 1 && cs === 0 =>
          List(out - 1, 0, scs + 1)
      },
      { // 3
        case List(out, cs, scs) if cs >= 1 =>
          List(out + 1, cs - 1, scs)
      },
      { // 4
        case List(out, cs, scs) if scs >= 1 =>
          List(out + 1, cs, scs - 1)
      },
    )

  def unsafe(c: Conf) = c match {
    case List(out, cs, scs) if cs >= 1 && scs >= 1 => true
    case _                                         => false
  }

  override val name = "datarace"

  override val isabelleEncoding: String =
    """
    |inductive datarace :: "(nat * nat * nat) => bool" where
    |  "datarace (out, 0, 0)" |
    |  "datarace (Suc out, 0, 0) ==> datarace (out, Suc 0, 0)" |
    |  "datarace (Suc out, 0, scs) ==> datarace (out, 0, Suc scs)" |
    |  "datarace (out, Suc cs, scs) ==> datarace (Suc out, cs, scs)" |
    |  "datarace (out, cs, Suc scs) ==> datarace (Suc out, cs, scs)"
    |
    |inductive unsafe :: "(nat * nat * nat) => bool" where 
    |  "unsafe (out, Suc cs, Suc scs)"
    """.stripMargin

}
