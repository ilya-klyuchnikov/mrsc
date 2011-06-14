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
          val rollback = Rollback(blamed, topSplitted, NoExtra)
          List(rollback)
        } else if (g.t.isInstanceOf[Var]) {
          val let = split(currentConf)
          val replace = Replace(let, NoExtra)
          List(replace)
        } else {
          val let = Let(g.t, g.m1.toList)
          val replace = Replace(let, NoExtra)
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
          val replace = Replace(let, NoExtra)
          //println("replace: " + let)
          List(replace)
        } else {
          val let = Let(g.t, g.m1.toList)
          val rollback = Rollback(blamed, let, NoExtra)
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
          Rollback(blamed, let, NoExtra)
        }

        val downMsg = if (renaming(g.t, currentConf)) {
          null
        } else {
          val let = Let(g.t, g.m1.toList)
          //println(let)
          //println("----")
          Replace(let, NoExtra)
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
        
        mutualSupers1 map { Replace(_, NoExtra) }
    }
}

trait SLLAlwaysCurrentGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] = {
    val expr = pState.node.conf
    gens(expr) map { Replace(_, NoExtra) }
  }
}

trait SLLWhistleCurrentGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle match {
      case None =>
        List()
      case Some(blamed) =>
        val expr = pState.node.conf
        gens(expr) map { Replace(_, NoExtra) }
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
        gs map { Rollback(blamed, _, NoExtra) }
    }
}

// if generalization of blamed configuration is empty,
// then try ALL generalizations of the current configuration.
trait SLLWhistleBlamedGens2 extends SLLRebuildings {
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
        println("***")
        val rollbacks = SLLGeneralizations.gens(blamedExpr) map {
          Rollback(blamed, _, NoExtra)
        }
        if (!rollbacks.isEmpty) {
          rollbacks
        } else {
          val current = SLLGeneralizations.gens(currentExpr) map {
            Replace(_, NoExtra)
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
          Rollback(blamed, _, NoExtra)
        }
        val current = SLLGeneralizations.gens(currentExpr) map {
          Replace(_, NoExtra)
        }
        rollbacks ++ current
    }
}

trait SLLNoTricks {
  def tricks(whistle: SLLSignal, pState: SLLState) =
    Nil
}
