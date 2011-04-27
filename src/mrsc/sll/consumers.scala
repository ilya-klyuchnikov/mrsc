package mrsc.sll

import mrsc._

// it just counts trees
class CountGraphConsumer extends CoGraphConsumer[Expr, Contraction] {
  val description = "counting completed and pruned graphs"
  var completed = 0
  var pruned = 0

  def consume(result: Option[CoGraph[Expr, Contraction]]): Unit = {
    result match {
      case None => pruned = pruned + 1
      case Some(cg) => completed = completed + 1
    }
    if (completed > 50000 || pruned > 50000) {
      throw new ModelingError("to many results")
    }
  }

  def showResults(): Unit = {
    println(completed + " completed graphs")
    println(pruned + " pruned graphs")
  }
}

class CountProgramConsumer extends CoGraphConsumer[Expr, Contraction] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedCoGraphsCount = 0
  var prunedCoGraphsCount = 0

  var coGraphs: List[CoGraph[Expr, Contraction]] = Nil
  var programs: List[NExpr] = Nil

  var residualPrograms: List[NExpr] = Nil

  def consume(result: Option[CoGraph[Expr, Contraction]]): Unit = {
    result match {
      case None =>
        prunedCoGraphsCount = prunedCoGraphsCount + 1
      case Some(cg) =>
        completedCoGraphsCount = completedCoGraphsCount + 1
        coGraphs = cg :: coGraphs
    }
    if (completedCoGraphsCount > 1000 || prunedCoGraphsCount > 1000) {
      throw new ModelingError("to many results")
    }
  }

  def showResults(): Unit = {
    println(completedCoGraphsCount + " completed graphs")
    println(prunedCoGraphsCount + " pruned graphs")
    //if (completedCoGraphsCount < 100) {
    val allProgs = for (cg <- coGraphs; graph = Transformations.transpose(cg)) yield new NSLLResiduator(graph).result
    programs = allProgs.sortBy(_.size).distinct

    println(programs.length + " programs")

    println("""showing first 10 "minimal" programs""")
    for (p <- programs.take(10)) {
      println(p)
      println()
    }
    //}

  }
}

class CountProgramConsumer2 extends CoGraphConsumer[Expr, SubStepInfo] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedCoGraphsCount = 0
  var prunedCoGraphsCount = 0

  var coGraphs: List[CoGraph[Expr, SubStepInfo]] = Nil
  var programs: List[NExpr] = Nil

  var residualPrograms: List[NExpr] = Nil

  def consume(result: Option[CoGraph[Expr, SubStepInfo]]): Unit = {
    result match {
      case None =>
        prunedCoGraphsCount = prunedCoGraphsCount + 1
      case Some(cg) =>
        completedCoGraphsCount = completedCoGraphsCount + 1
        coGraphs = cg :: coGraphs
    }
    if (completedCoGraphsCount > 100000) {
      throw new ModelingError("to many results")
    }
  }

  def showResults(): Unit = {
    println(completedCoGraphsCount + " completed graphs")
    println(prunedCoGraphsCount + " pruned graphs")
    
    /*
    for (cg <- coGraphs; graph = Transformations.transpose(cg)) {
      println(graph)
    }*/
    
    val allProgs = for (cg <- coGraphs; graph = Transformations.transpose(cg)) yield (new NSLLResiduator2(graph).result, graph)
    val mapProg = Map(allProgs:_*)
    programs = allProgs map {_._1} sortBy{_.size} distinct

    println(programs.length + " programs")

    println("""showing first 10 "minimal" programs""")
    for (p <- programs.take(10)) {
      println(p)
      //println("============")
      //println(mapProg(p))
      //println()
    }

  }
}