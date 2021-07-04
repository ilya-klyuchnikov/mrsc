package mrsc.counters

import mrsc.core._

// Generates Isabelle proof using a graph of configurations.
object ProofGenerator {
  def generate(protocol: Protocol, graph: SGraph[Conf, _]): String = {
    val confs: List[Conf] = graph.completeNodes.map(_.conf).distinct
    val baseConfs: List[Conf] = confs.filter { c1 => confs.forall { c2 => c1 == c2 || !Conf.instanceOf(c1, c2) } }

    val setType = protocol.start.map { _ => "nat" } mkString ("(", " * ", ")")
    val states = baseConfs.map { c => """"%s'%s"""".format(protocol.name, conf2Isb(c)) } mkString (" |\n  ")

    val intro =
      """
      |theory %s
      |imports Main
      |begin
      |%s
      """.stripMargin.format(protocol.name, protocol.isabelleEncoding)

    val protocolGen =
      """
      |inductive %s' :: "%s => bool" where
      |  %s
      """.stripMargin.format(protocol.name, setType, states)

    val proof =
      """
      |lemma inclusion: "%s c ==> %s' c"
      |  apply(erule %s.induct)
      |  apply(erule %s'.cases | simp add: %s'.intros)+
      |done
      |
      |lemma safety: "%s' c ==> ~unsafe c"
      |  apply(erule %s'.cases)
      |  apply(erule unsafe.cases | auto)+
      |done
      |
      |theorem valid: "%s c ==> ~unsafe c"
      |  apply(insert inclusion safety, simp)
      |done
      |
      |end
      """.stripMargin.replace("%s", protocol.name)

    intro + protocolGen + proof
  }

  // utility method
  private def conf2Isb(c: Conf): String = c.map(expr2Isb).mkString("(", ", ", ")")

  // utility method
  private def expr2Isb(e: Expr): String = e match {
    case Omega           => "_"
    case Num(i) if i < 0 => expr2Isb(Num(-i))
    case Num(1)          => "Suc 0"
    case Num(0)          => "0"
    case Num(i)          => "Suc (%s)".format(expr2Isb(Num(i - 1)))
  }
}
