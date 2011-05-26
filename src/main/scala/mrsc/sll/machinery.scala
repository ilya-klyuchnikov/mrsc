package mrsc.sll

import mrsc._
import Signal._
import SLLGeneralizations._
import SLLExpressions._

// Here we define 2 variants of driving + 6 variants of rebuilding
// TODO Really this part can be generalized: it should not-be SLL-specific.

trait SLLSimpleDriving extends SLLDriving {
  def drive(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List(drive(pState))
      case _ =>
        List()
    }
}

trait SLLPruningDriving extends SLLDriving {
  def drive(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List(drive(pState))
      case _ =>
        List(Prune)
    }
}

trait SLLCurentMsg extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamed = whistle.blamed.get
        val currentConf = pState.node.conf
        val blamedConf = blamed.conf
        val g = MSG.msg(currentConf, blamedConf)
        if (renaming(g.t, currentConf)) {
          val topSplitted = split(blamedConf)
          val rollback = Rollback(blamed, topSplitted, DummyExtra)
          List(rollback)
        } else if (g.t.isInstanceOf[Var]) {
          val let = split(currentConf)
          val replace = Replace(let, DummyExtra)
          List(replace)
        } else {
          val let = Let(g.t, g.m1.toList)
          val replace = Replace(let, DummyExtra)
          List(replace)
        }
    }
}

trait SLLBlamedMsg extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamed = whistle.blamed.get
        val currentConf = pState.node.conf
        val blamedConf = blamed.conf
        //println("msg")
        //println(blamedConf + "<" + currentConf)
        val g = MSG.msg(blamedConf, currentConf)
        if (g.t.isInstanceOf[Var] || renaming(g.t, blamedConf)) {
          val let = split(currentConf)
          val replace = Replace(let, DummyExtra)
          //println("replace: " + let)
          List(replace)
        } else {
          val let = Let(g.t, g.m1.toList)
          val rollback = Rollback(blamed, let, DummyExtra)
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
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamed = whistle.blamed.get
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
          Rollback(blamed, let, DummyExtra)
        }

        val downMsg = if (renaming(g.t, currentConf)) {
          null
        } else {
          val let = Let(g.t, g.m1.toList)
          //println(let)
          //println("----")
          Replace(let, DummyExtra)
        }

        //println()
        List(topMsg, downMsg).filter(_ != null)
    }
}

// tries all mutual superconfiguration for blamed
// and current configurations
trait SLLMix extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamed = whistle.blamed.get
        val currentConf = pState.node.conf
        val blamedConf = blamed.conf
        
        val mutualSupers1 = gens(currentConf)
        
        mutualSupers1 map { Replace(_, DummyExtra) }
    }
}

trait SLLAlwaysCurrentGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] = {
    val expr = pState.node.conf
    gens(expr) map { Replace(_, DummyExtra) }
  }
}

trait SLLWhistleCurrentGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val expr = pState.node.conf
        gens(expr) map { Replace(_, DummyExtra) }
    }
}

trait SLLWhistleBlamedGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamedNode = whistle.blamed.get
        val blamedExpr = blamedNode.conf
        val currentExpr = pState.node.conf
        println("generalizing")
        println(blamedExpr)
        println(currentExpr)
        val gs = gens(blamedExpr)
        gs.foreach(println)
        println("***")
        gs map { Rollback(blamedNode, _, DummyExtra) }
    }
}

// if generalization of blamed configuration is empty,
// then try ALL generalizations of the current configuration.
trait SLLWhistleBlamedGens2 extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamedNode = whistle.blamed.get
        val blamedExpr = blamedNode.conf
        val currentExpr = pState.node.conf
        println("generalizing")
        println(blamedExpr)
        println(currentExpr)
        println("***")
        val blamed = SLLGeneralizations.gens(blamedExpr) map {
          Rollback(blamedNode, _, DummyExtra)
        }
        if (!blamed.isEmpty) {
          blamed
        } else {
          val current = SLLGeneralizations.gens(currentExpr) map {
            Replace(_, DummyExtra)
          }
          current
        }
    }
}

trait SLLWhistleAllGens extends SLLRebuildings {
  def rebuildings(whistle: SLLSignal, pState: SLLState): List[SLLStep] =
    whistle.signal match {
      case OK =>
        List()
      case _ =>
        val blamedNode = whistle.blamed.get
        val blamedExpr = blamedNode.conf
        val currentExpr = pState.node.conf
        val blamed = SLLGeneralizations.gens(blamedExpr) map {
          Rollback(blamedNode, _, DummyExtra)
        }
        val current = SLLGeneralizations.gens(currentExpr) map {
          Replace(_, DummyExtra)
        }
        blamed ++ current
    }
}

trait SLLNoTricks {
  def tricks(whistle: SLLSignal, pState: SLLState) =
    Nil
}
