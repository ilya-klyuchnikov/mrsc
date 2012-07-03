package mrsc

package object pfp {
  // Field in constructor
  type Field = (String, Term)

  // Branch in case expression
  type Branch = (Ptr, Term)

  // global context
  type GContext = Map[String, Term]

  // substitution
  type Subst = Map[FVar, Term]

  implicit def t(s: String): Term =
    PFPParsers().inputTerm(s)

  implicit def bc(s: String): GContext =
    PFPParsers().inputBindings(s)

}