package mrsc.sll

import mrsc._
import SLLGeneralizations._
import SLLExpressions._

// Here we define 2 variants of driving + 6 variants of rebuilding
// TODO Really this part can be generalized: it should not-be SLL-specific.

trait SLLCurentMsg extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val currentConf = pState.node.conf
        val blamedConf = blamed.conf
        val g = MSG.msg(currentConf, blamedConf)
        if (renaming(g.t, currentConf)) {
          val topSplitted = split(blamedConf)
          val rollback = RollbackSubGraph(blamed, topSplitted, NoExtra)
          List(rollback)
        } else if (g.t.isInstanceOf[Var]) {
          val let = split(currentConf)
          val replace = ReplaceNode(let, NoExtra)
          List(replace)
        } else {
          val let = Let(g.t, g.m1.toList)
          val replace = ReplaceNode(let, NoExtra)
          List(replace)
        }
    }
}

trait SLLBlamedMsg extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val currentConf = pState.node.conf
        val blamedConf = blamed.conf
        //println("msg")
        //println(blamedConf + "<" + currentConf)
        val g = MSG.msg(blamedConf, currentConf)
        if (g.t.isInstanceOf[Var] || renaming(g.t, blamedConf)) {
          val let = split(currentConf)
          val replace = ReplaceNode(let, NoExtra)
          //println("replace: " + let)
          List(replace)
        } else {
          val let = Let(g.t, g.m1.toList)
          val rollback = RollbackSubGraph(blamed, let, NoExtra)
          //println("rollback: " + let)
          List(rollback)
        }
    }
}

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

// tries all mutual superconfiguration for blamed
// and current configurations
trait SLLMix extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val currentConf = pState.node.conf
        val blamedConf = blamed.conf
        
        val mutualSupers1 = gens(currentConf)
        
        mutualSupers1 map { ReplaceNode(_, NoExtra) }
    }
}

trait SLLAlwaysCurrentGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] = {
    val expr = pState.node.conf
    gens(expr) map { ReplaceNode(_, NoExtra) }
  }
}

trait SLLWhistleCurrentGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val expr = pState.node.conf
        gens(expr) map { ReplaceNode(_, NoExtra) }
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

trait SLLNoTricks {
  def tricks(whistle: SLLSignal, pState: SLLState) =
    Nil
}
