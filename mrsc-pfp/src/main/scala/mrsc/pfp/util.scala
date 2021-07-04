package mrsc.pfp

import mrsc.core._

import javax.swing.JFrame
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxStylesheet
import com.mxgraph.util.mxConstants
import java.awt.event.{WindowEvent, WindowAdapter}

// pretty printing with shows
trait PFPGraphPrettyPrinter {
  import scalaz._
  import Scalaz._
  implicit def termShow[T <: MetaTerm]: Show[T]

  def toString(tg: TGraph[MetaTerm, Label]): String = {
    val focus = tg.focus
    toString(tg.root, focus)
  }

  def toStringDense(tg: TGraph[MetaTerm, Label]): String = {
    val focus = tg.focus
    toStringDense(tg.root, focus)(tg)
  }

  def labelToString(l: Label): String =
    l match {
      case TransientLabel                 => "->"
      case UnfoldLabel                    => "->*"
      case CaseBranchLabel(sel, ptr, alt) => sel.shows + " = " + alt.shows
      case _                              => ""
    }

  def toString(node: TNode[MetaTerm, Label], focus: Option[TPath], indent: String = ""): String = {
    val sb = new StringBuilder()

    sb.append(indent + "|__" + node.conf.shows)
    if (node.base.isDefined) {
      sb.append("*")
    }
    if (focus == Some(node.tPath)) {
      sb.append(" <===")
    }
    for (edge <- node.outs) {
      sb.append("\n  " + indent + "|" + labelToString(edge.driveInfo))
      sb.append("\n" + toString(edge.node, focus, indent + "  "))
    }
    sb.toString
  }

  // not showing transient reductions
  def toStringDense(node: TNode[MetaTerm, Label], focus: Option[TPath], indent: String = "")(implicit
      g: TGraph[MetaTerm, Label]
  ): String = {

    node.outs match {
      case TEdge(n, UnfoldLabel) :: Nil if !g.leaves.exists(_.base == Some(node.tPath)) =>
        toStringDense(n, focus, indent)
      case TEdge(n, TransientLabel) :: Nil if !g.leaves.exists(_.base == Some(node.tPath)) =>
        toStringDense(n, focus, indent)
      case _ =>
        val sb = new StringBuilder()
        sb.append(indent + "|__" + node.conf.shows)
        if (node.base.isDefined) {
          sb.append("*")
        }
        if (focus == Some(node.tPath)) {
          sb.append(" <===")
        }
        for (edge <- node.outs) {
          sb.append("\n  " + indent + "|" + labelToString(edge.driveInfo))
          sb.append("\n" + toStringDense(edge.node, focus, indent + "  "))
        }
        sb.toString
    }

  }
}

trait PFPGraphUI {

  import scalaz._
  import Scalaz._

  import com.mxgraph.view.mxGraph
  import com.mxgraph.layout.hierarchical.mxHierarchicalLayout

  implicit def termShow[T <: MetaTerm]: Show[T]

  def labelToString(l: Label): String =
    l match {
      case TransientLabel                 => "->"
      case UnfoldLabel                    => "->*"
      case CaseBranchLabel(sel, ptr, alt) => sel.shows + " = " + alt.shows
      case _                              => ""
    }

  def createMxGraph(tg: TGraph[MetaTerm, Label]): mxGraph = {
    val style = new mxStylesheet
    style.getDefaultEdgeStyle.put(mxConstants.STYLE_SPACING, "20")
    style.getDefaultVertexStyle.put(mxConstants.STYLE_FONTFAMILY, "Courier")
    style.getDefaultVertexStyle.put(mxConstants.STYLE_FONTSIZE, "12")
    style.getDefaultEdgeStyle.put(mxConstants.STYLE_FONTFAMILY, "Courier")
    val graph: mxGraph = new mxGraph(style)
    val map = scala.collection.mutable.Map[TPath, AnyRef]()
    val gparent = graph.getDefaultParent

    def createNode(node: TNode[MetaTerm, Label]): AnyRef = {
      val text = node.conf.shows
      val v = graph.insertVertex(gparent, null, text, 0, 0, 7 * text.length + 10, 30)
      node.conf match {
        case x: Rebuilding =>
          graph.getModel.setStyle(v, "fillColor=white;")
        case _ =>
      }
      map(node.tPath) = v
      for (edge <- node.outs) {
        val v1 = createNode(edge.node)
        graph.insertEdge(gparent, null, labelToString(edge.driveInfo), v, v1)
      }
      node.base.foreach { p =>
        graph.insertEdge(gparent, null, "  ", map(p), v, "endArrow=none;strokeColor=red;")
        graph.getModel.setStyle(v, "strokeColor=red;")
        graph.getModel.setStyle(map(p), "strokeColor=red;")
      }
      v
    }

    graph.getModel.beginUpdate
    createNode(tg.root)
    val layout = new mxHierarchicalLayout(graph)
    layout.execute(graph.getDefaultParent)
    graph.getModel.endUpdate

    graph
  }

  var opened = 0

  object closer extends WindowAdapter {
    override def windowClosing(e: WindowEvent) {
      opened -= 1
      if (opened == 0) {
        System.exit(0)
      }
    }
  }

  def showMxGraph(tg: TGraph[MetaTerm, Label], autoExit: Boolean = true) {
    val frame = new JFrame("mrsc")
    frame.getContentPane.add(new mxGraphComponent(createMxGraph(tg)))
    frame.setSize(800, 600)
    frame.setVisible(true)
    if (autoExit) {
      frame.addWindowListener(closer)
      opened += 1
    }
  }
}

object NatRepl {
  val bindings = io.bingingsFromFile("pfp/defs/nat.pfp")

  def main(args: Array[String]) {
    val in = args(0)
    val goal = PFPParsers().inputTerm(in)
    val rules = new InteractiveMRSC(bindings)
    val sGraph = GraphGenerator(rules, goal).toList.head

    val tGraph = Transformations.transpose(sGraph)
    val simpleResidual = Residuator(tGraph).result
    val tickedResidual = Residuator(tGraph, true).result

    println(rules.prettyPrinter.toString(tGraph))
    println(rules.userSteps)

    println(NamedSyntax.named(simpleResidual))
    println(NamedSyntax.named(tickedResidual))
  }

}
