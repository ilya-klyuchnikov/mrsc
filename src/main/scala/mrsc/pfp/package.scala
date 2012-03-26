package mrsc

package object pfp {
  // Field in constructor
  type Field = (String, Term)

  // Branch in case expression
  type Branch = (String, Term)

  // global context
  type GContext = Map[String, Term]
}