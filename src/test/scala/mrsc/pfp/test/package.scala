package mrsc.pfp

package object test {
  implicit def t(s: String): Term =
    PFPParsers().inputTerm(s)
    
  implicit def bc(s: String): GContext =
    PFPParsers().inputBindings(s)
}