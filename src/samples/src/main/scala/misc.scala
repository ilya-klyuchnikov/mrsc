import mrsc.pfp._
import mrsc.core._

package object misc {
  type SC = GContext => PFPRules
}

package misc {

import mrsc.core.{SGraph, Transformations, GraphGenerator}
import annotation.tailrec

// checks the correctness of simple programs
// assumes that all free variables in a program are natural numbers
// and that a program terminates
object SimpleChecker {

  def run(f: String, sc: PFPSC) {
    import scala.io.Source
    val text = Source.fromFile(f).mkString
    val task0 = PFPParsers().inputTask(text)
    val task = task0.copy(name = f)
    Console.println(text)
    Console.println(task.goal)

    val rules = sc(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, false)

    var checked = 0
    for { sGraph <- graphs } {
      val tGraph = Transformations.transpose(sGraph)
      val result = Residuator(tGraph).result
      checked += 1
      check(task, Task(result, Map()))
    }

    Console.println("OK, checked %d results".format(checked))
  }

  def check(original: Task, transformed: Task, seed: Int = 2) {
    import NamelessSyntax._

    val fvs = freeVars(original.goal)
    val subs = allSubs(fvs, seed)

    for {sub <- subs} {
      val val1 = CBNEval.eval(applySubst(original.goal, sub), original.bindings)
      val val2 = CBNEval.eval(applySubst(transformed.goal, sub), transformed.bindings)
      assert(val1 == val2)
      Console.println(val1)
    }
  }

  // all possible subs
  def allSubs(fvs: List[FVar], seed: Int): List[Subst] =
    cartesianProduct(fvs.map(_ => values(seed))) map {vs => Map(fvs zip vs:_*)}

  def values(seed: Int): List[Term] =
    (0 to seed).toList.map(nat)

  def nat(i: Int): Term =
    if (i == 0) Ctr("Z", Nil) else Ctr("S", nat(i - 1) :: Nil)

  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for(xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
  }
}

// REPL for interactive experiments with
// single-result supercompilers
object SREPL {

  def sc(sc: SC, t: String) {
    if (!tasks.tasks.get(t).isDefined) {
      Console.println("no such task")
      return
    }
    val task = tasks(t)
    runTask(sc, task)
  }

  private def runTask(sc: SC, task: Task) {
    Console.println(task.name)
    Console.println(task.goal)

    val rules = sc(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, true).toList
    val sGraph = graphs.head
    val tGraph = Transformations.transpose(sGraph)

    history = history(sGraph).reverse
    current = -1

    Console.println(tGraph)
    val result = Residuator(tGraph).result
    Console.println(result)
    Console.println(NamedSyntax.named(result))
  }

  var current = 0
  var history: List[SGraph[_, _]] = _

  @tailrec
  def trace() {
    Console.readLine() match {
      case "" if current < history.size - 1 =>
        clear()
        current += 1
        val sGraph = history(current)
        val tGraph = Transformations.transpose(sGraph)
        Console.println(tGraph.toString)
      case ":p" if current > 0 =>
        clear()
        current -= 1
        val sGraph = history(current)
        val tGraph = Transformations.transpose(sGraph)
        Console.println(tGraph.toString)
      case _ =>
        return
    }
    trace()
  }

  private def clear() {
    Console.println("\033[2J")
    Console.println("\033[0;0H")
    Console.flush()
  }

  def ls() {
    val ts = tasks.tasks.values.toList.sortBy(_.name)
    ts map { t => Console.println(t.name + ": " + t.goal) }
  }

  def scAll(sc: SC) {
    val ts = tasks.tasks.values.toList.sortBy(_.name)
    ts map { runTask(sc, _) }
  }

  def history(g: SGraph[_, _]): List[SGraph[_, _]] =
    g :: (g.prev.map(history).getOrElse(Nil))
}

object tasks {

  var tasks = Map[String, Task]()

  def apply(name: String): Task = tasks(name)

  task(
    "lam1",
    """\x -> x""",
    "")

  task(
    "app1", "app <1> <2>",

    """
      app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, (app xs y))
        };
    """)

  // transforming supercompiled program!
  task(
    "app2",
    """
        letrec h = \x -> \y ->
          case x of {
            Nil()  -> y;
            Cons(x, xs) -> Cons(x, h xs y)
          } in h <1> <2>
    """,
    "")

  // repeated variable
  task(
    "app3", "app <1> <1>",

    """
      app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x, xs) -> Cons(x, (app xs y))
        };
    """)

  // generalizing supercompiled program!
  task(
    "app4",
    """
        letrec h = \x -> \y ->
          case x of {
            Nil()  -> y;
            Cons(x, xs) -> Cons(x, h xs y)
          } in h <1> <1>
    """,
    "")

  task(
    "fix1",
    """fix (\a -> A(a))""",
    """
      fix = \f -> f (fix f);
    """)

  task(
    "fix2",
    """fst(fix (\r -> P( A(snd r), B(fst r) )))""",
    """
      fst = \p -> case p of { P(x, y) -> x };
      snd = \p -> case p of { P(x, y) -> y };
      fix = \f -> f(fix f);
    """)

  task(
    "fix3",
    """fst (fix (\r -> t2 (A (snd r)) (B (fst r))))""",
    """
      t2 = \x -> \y -> \f -> f x y;
      fst = \f -> f (\x -> \y -> x);
      snd = \f -> f (\x -> \y -> y);
      fix = \f -> f(fix f);
    """)

  task(
    "iterate",
    """iterate (\n -> S(n)) <1>""",
    """
      iterate = \f -> \x -> Cons(x, (iterate f (f x)));
    """)

  task(
    "rev",
    """rev <1> """,
    """
      rev = \x ->
        case x of {
          Nil() -> Nil();
          Cons(x1, xs) -> app (rev xs) Cons(x1, Nil())
      };
      app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x1, xs) -> Cons(x1, (app xs y))
        };
    """)

  task(
    "rev1",
    """case
         case <5> of {Cons(p, q) -> app (rev q) Cons(p, Nil()); Nil() -> Nil()}
         of { Cons(r, s) -> Cons(r, (app s Cons(<4>, Nil() ))); Nil() -> Cons(<4>, Nil())}
    """,
    """
      rev = \x ->
        case x of {
          Nil() -> Nil();
          Cons(x1, xs) -> app (rev xs) Cons(x1, Nil())
      };
      app = \x -> \y ->
        case x of {
          Nil()  -> y;
          Cons(x1, xs) -> Cons(x1, (app xs y))
        };
    """)

  private def task(name: String, goal: Term, bindings: GContext): Unit = {
    val t = Task(goal, bindings, name)
    tasks = tasks + (t.name -> t)
  }

}

case class SC1(val gc: GContext) extends PFPRules
with PFPSemantics
with PositiveDriving
with AllFoldingCandidates
with Folding
with AllEmbeddingCandidates
with HE3ByCouplingWhistle
with UpperMsgOrLowerMggOnBinaryWhistle

object SC1 extends SC

case class SC2(val gc: GContext) extends PFPRules
with PFPSemantics
with PositiveDriving
with AllFoldingCandidates
with Folding
with ControlEmbeddingCandidates
with HE3ByCouplingWhistle
with LowerMsgOrUpperMsgOnBinaryWhistle

object SC2 extends SC

case class SC3(val gc: GContext) extends PFPRules
with PFPSemantics
with PositiveDriving
with AllFoldingCandidates
with Folding
with ControlEmbeddingCandidates
with HE3ByCouplingWhistle
with UpperMsgOrLowerMsgOnBinaryWhistle

object SC3 extends SC

case class AllMSC(val gc: GContext) extends PFPRules
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3Whistle
  with AllRebuildings

object AllMSC extends SC

// All rebuildings but only on whistle
case class MSC1(val gc: GContext) extends PFPRules
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with HE3Whistle
  with LowerRebuildingsOnBinaryWhistle

object MSC1 extends SC

case class MSC2(val gc: GContext) extends PFPRules
with PFPSemantics
with Driving
with AllFoldingCandidates
with Folding
with AllEmbeddingCandidates
with HE3Whistle
with LowerAllBinaryGensOnBinaryWhistle

object MSC2 extends SC

case class MSC3(val gc: GContext) extends PFPRules
//with PFPSyntax
with PFPSemantics
with Driving
with AllFoldingCandidates
with Folding
with AllEmbeddingCandidates
with HE3ByCouplingWhistle
with LowerAllBinaryGensOnBinaryWhistle

object MSC3 extends SC

case class MSC4(val gc: GContext) extends PFPRules
with PFPSemantics
with Driving
with AllFoldingCandidates
with Folding
with AllEmbeddingCandidates
with HE3ByCouplingWhistle
with UpperAllBinaryGensOnBinaryWhistle

object MSC4 extends SC

case class MSC5(val gc: GContext) extends PFPRules
with SizedRebuildingsGenerator
with PFPSemantics
with Driving
with AllFoldingCandidates
with Folding
with AllEmbeddingCandidates
with HE3Whistle
with AllRebuildings {

  val genSize = 1
}

object MSC5 extends SC

case class MSC6(val gc: GContext) extends PFPRules
with SizedRebuildingsGenerator
with PFPSemantics
with Driving
with AllFoldingCandidates
with Folding
with AllEmbeddingCandidates
with HE3ByCouplingWhistle
with UpperAllBinaryGensOnBinaryWhistle {
  val genSize = 2
}


object MSC6 extends SC

case class AllMSC1(val gc: GContext) extends PFPRules
with PFPSemantics
with Driving
with AllFoldingCandidates
with Folding
with AllEmbeddingCandidates
with HE3Whistle
with AllRebuildings
with SizeGraphFilter {
  val maxGraphSize = 20
}

object AllMSC1 extends SC

object EmbeddingsDetectors {
  case class SC1(val gc: GContext) extends PFPRules
  with PFPSemantics
  with PositiveDriving
  with AllEmbeddingCandidates
  with HE3ByCouplingWhistle
  with FoldingOnBinaryWhistle

  object SC1 extends PFPSC

  case class SC2(val gc: GContext) extends PFPRules
  with PFPSemantics
  with PositiveDriving
  with ControlEmbeddingCandidates
  with HE3ByCouplingWhistle
  with FoldingOnBinaryWhistle

  object SC2 extends PFPSC
}

object repl {

  import scalaz._
  import Scalaz._
  import NamelessShows._

  val mxUI = new PFPGraphUI {
    implicit def termShow[T <: MetaTerm]: Show[T] = NamelessShows.TermShow
  }

  def graphs(file: String, sc: PFPSC): Iterator[SGraph[MetaTerm, Label]] = {
    val (_, task) = io.taskFromFile(file)
    val rules = sc(task.bindings)
    GraphGenerator(rules, task.goal, false)
  }

  val prettyPrinter = new PFPGraphPrettyPrinter {
    implicit def termShow[T <: MetaTerm]: Show[T] = NamelessShows.TermShow
  }

  def showGraphs(file: String, sc: PFPSC) =
    for {sGraph <- graphs(file, sc)} {
      val tGraph = Transformations.transpose(sGraph)
      Console.println(prettyPrinter.toString(tGraph))
    }

  def showFoldings(file: String, sc: PFPSC) =
    graphs(file, sc).zipWithIndex.foreach { case (sGraph, i) =>
      Console.println("### graph %s ###".format(i + 1))
      sGraph.completeLeaves.foreach {
        case leaf@SNode(_, _, Some(ePath), _) =>
          val Some(embedded) = leaf.ancestors.find(_.sPath == ePath)
          Console.println(embedded.conf.shows)
          Console.println("<*>")
          Console.println(leaf.conf.shows)
          Console.println()
        case _ =>
      }
    }

  def showResiduals(file: String, sc: PFPSC, showGraphs: Boolean = false) =
    graphs(file, sc).zipWithIndex.foreach { case (sGraph, i) =>
      val tGraph = Transformations.transpose(sGraph)
      if (showGraphs) {
        mxUI.showMxGraph(tGraph)
      }
      val res = Residuator(tGraph).result
      Console.println(res.shows)
      Console.println(NamedSyntax.named(res))
    }
}

// For performing different experiments with multi-result supercompilers
object multi {

  case class AllMSC1(val gc: GContext, val maxGraphSize: Int) extends PFPRules
  with PFPSemantics
  with Driving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with NoWhistle
  with AllRebuildings
  with SizeGraphFilter

  // it doesn't allow to substitute let-expressions during residualization
  case class AllMSC2(val gc: GContext, val maxGraphSize: Int) extends PFPRules
  with PFPSemantics
  with LetDriving
  with AllFoldingCandidates
  with Folding
  with AllEmbeddingCandidates
  with NoWhistle
  with AllRebuildings
  with SizeGraphFilter

  def allDepthBound1(depth: Int): PFPSC = gc => AllMSC1(gc, depth)

  def allDepthBound2(depth: Int): PFPSC = gc => AllMSC2(gc, depth)

  // shows all possible variants of supercompilation of a given program
  def run(f: String, sc: PFPSC) {
    import scala.io.Source
    val text = Source.fromFile(f).mkString
    val task = PFPParsers().inputTask(text)
    Console.println(text)
    Console.println(task.goal)

    val rules = sc(task.bindings)
    val graphs = GraphGenerator(rules, task.goal, false)

    var count = 0
    var uniques: Set[Term] = Set()
    for { sGraph <- graphs } {
      val tGraph = Transformations.transpose(sGraph)
      val result = Residuator(tGraph).result
      count += 1
      uniques = uniques + result
      //Console.println("%s/%s".format(count, uniques.size))
      //Console.println(tGraph)
      //Console.println(NamedSyntax.named(result))
      //Console.println()
    }

    val results = uniques.toList.sortBy(_.size)
    for { res <- results } {
      Console.println(res)
      Console.println(NamedSyntax.named(res))
      Console.println()
    }
  }

}


}
