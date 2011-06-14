package mrsc.sll

import mrsc._

// it just counts completed and pruned graphs
class CountGraphConsumer[C, D, E] extends CoGraphConsumer[C, D, E] {
  val description = "counting completed and pruned graphs"
  var completed = 0
  var pruned = 0

  def consume(result: Option[CoGraph[C, D, E]]): Unit = {
    result match {
      case None => pruned = pruned + 1
      case Some(cg) => completed = completed + 1
    }
    if (pruned % 1000 == 0 || completed % 1000 == 0) {
      println((completed, pruned))
    }
    if (completed > 10000000 || pruned > 10000000) {
      throw new ModelingError("too many results")
    }
  }

  def showResults(): Unit = {
    println(completed + " completed graphs")
    println(pruned + " pruned graphs")
  }
}

class CountProgramConsumer extends CoGraphConsumer[Expr, Contraction[Expr], Extra] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedCoGraphsCount = 0
  var prunedCoGraphsCount = 0

  var coGraphs: List[CoGraph[Expr, Contraction[Expr], Extra]] = Nil
  var programs: List[NExpr] = Nil

  var residualPrograms: List[NExpr] = Nil

  def consume(result: Option[CoGraph[Expr, Contraction[Expr], Extra]]): Unit = {
    result match {
      case None =>
        prunedCoGraphsCount = prunedCoGraphsCount + 1
      case Some(cg) =>
        completedCoGraphsCount = completedCoGraphsCount + 1
        coGraphs = cg :: coGraphs
    }
    if (completedCoGraphsCount > 1000 || prunedCoGraphsCount > 1000) {
      throw new ModelingError("too many results")
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
  }
}

class CountProgramConsumer2 extends CoGraphConsumer[Expr, DriveInfo[Expr], Extra] {
  val description = "counting completed and pruned graphs and showing residual programs"

  var completedCoGraphsCount = 0
  var prunedCoGraphsCount = 0

  var coGraphs: List[CoGraph[Expr, DriveInfo[Expr], Extra]] = Nil
  var programs: List[NExpr] = Nil

  var residualPrograms: List[NExpr] = Nil

  def consume(result: Option[CoGraph[Expr, DriveInfo[Expr], Extra]]): Unit = {
    result match {
      case None =>
        prunedCoGraphsCount = prunedCoGraphsCount + 1
      case Some(cg) =>
        completedCoGraphsCount = completedCoGraphsCount + 1
        coGraphs = cg :: coGraphs
    }
    if (completedCoGraphsCount > 10000) {
      throw new ModelingError("too many results")
    }
  }

  var nProgs: List[NExpr] = null
  lazy val sllTasks = nProgs map {NSLL.toSLL} 
  
  def showResults(): Unit = {
    
    val allProgs = for (cg <- coGraphs; graph = Transformations.transpose(cg)) yield (new NSLLResiduator2(graph).result, graph)
    nProgs = allProgs map {_._1}
    
    val mapProg = Map(allProgs: _*)
    programs = nProgs sortBy { _.size } distinct

    //println(programs.length + " programs")

    //val firsts = programs.take(40)
    //println("""showing first """ + firsts.length + """ "minimal" programs""")
    //for (p <- firsts) {
      //println(p)
      
      //val sll = NSLL.toSLL(p)
      //println("<<>>")
      //println(sll)
      //println("----")
      //println("============")
      //println(mapProg(p))
      //println()
    //}

  }
  
  def result(): String = {
    "" + programs.length
  }
}

class SingleProgramConsumer extends CoGraphConsumer[Expr, DriveInfo[Expr], Extra] {
  val description = "I expect one result"

  var residualProgram: NExpr = null
  var graph: Graph[Expr, DriveInfo[Expr], Extra] = null
  
  lazy val residualTask: SLLTask = NSLL.toSLL(residualProgram)

  def consume(result: Option[CoGraph[Expr, DriveInfo[Expr], Extra]]): Unit = {
    result match {
      case Some(cg) if residualProgram == null =>
        graph = Transformations.transpose(cg)
        residualProgram = new NSLLResiduator2(graph).result
      case _ =>
        throw new Error()
    }
  }

  def showResults(): Unit = {
    println(residualProgram)
    println("<<>>")
    //println(residualTask)
  }
}