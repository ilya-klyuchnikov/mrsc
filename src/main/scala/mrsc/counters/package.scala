package mrsc

package object counters {
  type OmegaConf = List[Component]
  type TransitionRule = PartialFunction[OmegaConf, OmegaConf]
  implicit def intToComponent(i: Int): Component = Value(i)
  val ϖ = Omega
}