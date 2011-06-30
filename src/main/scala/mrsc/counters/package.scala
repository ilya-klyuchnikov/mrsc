package mrsc

package object counters {
  type Counter = List[Component]
  type TransitionRule = PartialFunction[Counter, Counter]
  implicit def intToComponent(i: Int): Component = Value(i)
}