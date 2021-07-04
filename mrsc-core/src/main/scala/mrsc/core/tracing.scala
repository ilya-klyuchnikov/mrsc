package mrsc.core

object Tracing {

  def history(g: SGraph[_, _]): List[SGraph[_, _]] =
    g :: (g.prev.map(history).getOrElse(Nil))
}
