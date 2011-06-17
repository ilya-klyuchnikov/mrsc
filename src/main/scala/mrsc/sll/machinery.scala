package mrsc.sll

import mrsc._
import SLLGeneralizations._
import SLLExpressions._

// Currently it works only for coupling.
// This Msg works as follows:
// performs msg of the up expression wrt the down expression
// and performs msg of the down expression wrt the up expression
// when expressions are coupled, then at least one msg is non-trivial.
trait SLLMixMsg extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val currentConf = pState.node.conf
        val blamedConf = blamed.conf
        val g = MSG.msg(currentConf, blamedConf)
        require(!g.t.isInstanceOf[Var])

        //println("+++")
        //println(blamedConf)
        //println(currentConf)
        //println(">>>")

        val topMsg = if (renaming(g.t, blamedConf)) {
          null
        } else {
          val let = Let(g.t, g.m2.toList)
          //println(let)
          //println("----")
          RollbackSubGraph(blamed, let, NoExtra)
        }

        val downMsg = if (renaming(g.t, currentConf)) {
          null
        } else {
          val let = Let(g.t, g.m1.toList)
          //println(let)
          //println("----")
          ReplaceNode(let, NoExtra)
        }

        //println()
        List(topMsg, downMsg).filter(_ != null)
    }
}

trait SLLWhistleBlamedGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val blamedExpr = blamed.conf
        val currentExpr = pState.node.conf
        println("generalizing")
        println(blamedExpr)
        println(currentExpr)
        val gs = gens(blamedExpr)
        gs.foreach(println)
        println("***")
        gs map { RollbackSubGraph(blamed, _, NoExtra) }
    }
}

// if generalization of blamed configuration is empty,
// then try ALL generalizations of the current configuration.
trait SLLWhistleBlamedGens2 extends SLLRebuildings {
  val debug = false
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val blamedExpr = blamed.conf
        val currentExpr = pState.node.conf
        if (debug) {
          println("generalizing")
          println(blamedExpr)
          println(currentExpr)
          println("***")
        }
        val rollbacks = SLLGeneralizations.gens(blamedExpr) map { x =>
          if (debug) {println(x)}
          RollbackSubGraph(blamed, x, NoExtra)
        }
        if (debug) println(">>>")
        if (!rollbacks.isEmpty) {
          rollbacks
        } else {
          if (debug) println("UP empty, doing DOWN:")
          val current = SLLGeneralizations.gens(currentExpr) map { x =>
            if (debug) {println(x)}
            ReplaceNode(x, NoExtra)
          }
          current
        }
    }
}

trait SLLWhistleAllGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val blamedExpr = blamed.conf
        val currentExpr = pState.node.conf
        val rollbacks = SLLGeneralizations.gens(blamedExpr) map {
          RollbackSubGraph(blamed, _, NoExtra)
        }
        val current = SLLGeneralizations.gens(currentExpr) map {
          ReplaceNode(_, NoExtra)
        }
        rollbacks ++ current
    }
}
