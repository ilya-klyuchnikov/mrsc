package mrsc.pfp

package object test {
  implicit def t(s: String): Term =
    PFPParsers().inputTerm(s)
}