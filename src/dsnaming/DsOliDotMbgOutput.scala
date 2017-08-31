
/**
 *
 * Copyright (C) 2017 University of Bamberg, Software Technologies Research Group
 * <https://www.uni-bamberg.de/>, <http://www.swt-bamberg.de/>
 *
 * This file is part of the Data Structure Investigator (DSI) project, which received financial support by the
 * German Research Foundation (DFG) under grant no. LU 1748/4-1, see
 * <http://www.swt-bamberg.de/dsi/>.
 *
 * DSI is licensed under the GNU GENERAL PUBLIC LICENSE (Version 3), see
 * the LICENSE file at the project's top-level directory for details or consult <http://www.gnu.org/licenses/>.
 *
 * DSI is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or any later version.
 *
 * DSI is a RESEARCH PROTOTYPE and distributed WITHOUT ANY
 * WARRANTY, without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * The following people contributed to the conception and realization of the present DSI distribution (in
 * alphabetic order by surname):
 *
 * - Jan H. Boockmann
 * - Gerald Lüttgen
 * - Thomas Rupprecht
 * - David H. White
 *
 */
 
 /**
 * @author DSI
 *
 * DsOliDotMbgOutput.scala created on Oct 23, 2014
 *
 * Description: Produces the DOT files
 */
package dsnaming

import pointstograph.DsOliGraph
import scalax.collection.io.dot._
import pointstograph.DsOliDiEdge
import pointstograph.DsOliVertex
import pointstograph.DsOliVertexMemory
import extlogger.DsOliLogger
import pointstograph.DsOliVertexNull
import pointstograph.DsOliVertexUndef
import scalax.collection.GraphEdge.DiEdge
import pointstograph.DsOliVertexPredefined
import scala.util.control.Breaks._
import pointstograph.ITypeDB
import pointstograph.DsOliType
import pointstograph.FieldType._
import scalax.collection.Graph
import boxcalculation.DsOliBoxStep
import boxcalculation.DsOliBox
import boxcalculation.DsOliBox.BoxId
import scala.collection.mutable.ListBuffer
import boxcalculation.DsOliCell
import boxcalculation.DsOliCycle
import entrypoint.DsOliFeatureSet
import entrypoint.Feature
import util.DsOliAddressUtils
import entrypoint.DsOliEPT
import entrypoint.Feature._
import PartialFunction._
import scala.collection.mutable.HashMap
import dsnaming.DsOliConConfClassificationTag._
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.edge.LUnDiEdge
import util.DsOliGraphUtils
import dsnaming.DsOliConConfTag._
import boxcalculation.DsOliBoxSteps

/**
 * @author DSI
 *
 */
class DsOliDotMbgOutput {
  val classSignature = "DsOliDotMbgOutput::"

  val doubleEdges = new HashMap[(DsOliMetaBoxGraphVertex, DsOliMetaBoxGraphVertex), Boolean]()

  var graph: DsOliMetaBoxGraph = null
  var typeDB: ITypeDB = null 
  var boxSteps: DsOliBoxSteps = null

  val dotHeader = DotRootGraph(
    directed = true,
    id = Some("G"),
    kvList = Seq.empty)

  def calculateEdgeAttributes(vertex: DsOliVertex, label: String, edgeColor: String): List[scalax.collection.io.dot.DotAttr] = {
    vertex match {
      case v: DsOliVertexMemory =>
        //DsOliLogger.debug("DsOliDotGraphOutput::calculateEdgeAttributes: found memory vertex")

        if (label.nonEmpty) {
          List(DotAttr("color", "\"#" + edgeColor + "\""), DotAttr("label", label))
        } else {
          List(DotAttr("color", "\"#" + edgeColor + "\""))
        }
      case _ =>
        // Do not draw edges which are connected to e.g. NULL or UNDEF
        List(DotAttr("style", "invis"))
    }
  }

  def calculateFeaturesForEdge(sourceVertex: DsOliVertexMemory, innerEdge: scalax.collection.Graph[DsOliVertex, DsOliDiEdge]#EdgeT): String = {
    val funSignature = classSignature + "calculateFeaturesForEdge: "
    val edge = innerEdge.edge
    var retStr = ""
    retStr
  }

  /**
   * Convert an edge to HTML
   * @param innerEdge the edge to process
   * @return the HTML edge statement for DOT
   */
  def edgeTransformer(innerEdge: scalax.collection.Graph[DsOliMetaBoxGraphVertex, DsOliMetaBoxGraphDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
    val edgeOuter = innerEdge.toOuter
    val edge = edgeOuter.asInstanceOf[DsOliMetaBoxGraphDiEdge[DsOliMetaBoxGraphVertex]]
    val color = if (doubleEdges.contains((edge.source, edge.target))) {
      "#ff00ff"
    } else {
      this.doubleEdges.put((edge.source, edge.target), true)
      "\"#000000\""
    }

    val attributes = List(DotAttr("color", color), DotAttr("label", "<<TABLE BORDER=\"0\" ALIGN=\"left\" COLOR=\"black\" CELLBORDER=\"0\"  CELLPADDING=\"0\" CELLSPACING=\"0\"><TR><TD COLSPAN=\"2\">t: " +
      edgeOuter.startPoint + " # tag: " + (if (edgeOuter.conConf.tag == ccOverlay) "ovl" else "ptr") + "# offset: " + edgeOuter.conConf.offsets._1 + ":" + edgeOuter.conConf.offsets._2 + "</TD></TR>" +
      DsOliGraphUtils.compactCCClass(edge.conConfClass) + "</TABLE>>"))
    Some((dotHeader,
      DotEdgeStmt(
        "\"" + edge.source.toString + "\":\"" + edge.conConf.offsets._1 + edge.conConf.offsets._2 + "out:e\"",
        "\"" + edge.target.toString + "\":\"" + edge.conConf.offsets._1 + edge.conConf.offsets._2 + "in:w\"",
        attributes)))
  }

  /**
   * Convert a unidirectional edge into the DOT format
   * @param innnerEdge the edge to convert
   * @return the edge label
   */
  def edgeTransformerUnDi(innerEdge: scalax.collection.Graph[DsOliMetaBoxGraphVertex, UnDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
    val edgeOuter = innerEdge.toOuter
    val attributes = if (edgeOuter.isInstanceOf[DsOliMetaBoxGraphDiEdge[DsOliMetaBoxGraphVertex]]) {
      val edge = edgeOuter.asInstanceOf[DsOliMetaBoxGraphDiEdge[DsOliMetaBoxGraphVertex]]
      val color = DsOliGraphUtils.getColorForClass(DsOliGraphUtils.highestConConfClass(DsOliGraphUtils.conConfClassAgg(edge.conConfClass)).head)
      List(DotAttr("color", "\"" + color + "\""), DotAttr("label", "<<TABLE BORDER=\"0\" ALIGN=\"left\" COLOR=\"black\" CELLBORDER=\"0\"  CELLPADDING=\"0\" CELLSPACING=\"0\"><TR><TD COLSPAN=\"2\">t: " +
        edge.startPoint + " tag: " + (if (edge.conConf.tag == ccOverlay) "ovl" else "ptr") + " off: " + edge.conConf.offsets._1 + ":" + edge.conConf.offsets._2 + "</TD></TR>" +
        DsOliGraphUtils.compactCCClass(edge.conConfClass) + "</TABLE>>"))
      //"edge creation time = " + edge.startPoint + " cc = " + edge.conConf + ": ccClass = " + DsOliGraphUtils.compactCCClass(edge.conConfClass)))
    } else {
      val (edgeLabel, edgeClass) = edgeOuter.label.asInstanceOf[(String, scala.collection.mutable.Set[DsOliConConfClassification])]
      val color = DsOliGraphUtils.getColorForClass(DsOliGraphUtils.highestConConfClass(DsOliGraphUtils.conConfClassAgg(edgeClass)).head)
      List(DotAttr("dir", "none"), DotAttr("label", edgeLabel), DotAttr("color", "\"" + color + "\""))
    }
    Some((dotHeader,
      DotEdgeStmt(
        "\"" + edgeOuter._1.toString + "\"", // + "out:e\"",
        "\"" + edgeOuter._2.toString + "\"", // + "in:w\"",
        attributes)))
  }

  /**
   * [Deprecated] Directly prints nested nodes
   * @param node the node to print the nesting for
   * @param stop recursion depth
   * @return the created string
   */
  def printNodeWithNestingRec(node: DsOliMetaBoxGraphVertex, stop: Int): String = {
    var ret = node.name
    if (stop >= 10) {
      return ret + "emergency stop"
    }
    if (node.nesting.size > 0) {
      ret += " with nested "
      node.nesting.foreach {
        nestedNode =>
          ret += ":" + printNodeWithNestingRec(nestedNode, (stop + 1)) + ":,"
          if (stop >= 10) {
            return ret
          }
      }
    }
    ret
  }

  /**
   * Determine the longest strand within the given strand set
   *
   * @param boxes the strand set
   * @return the longest strand length
   */
  def getLongestStrandLength(boxes: Set[DsOliBox]): Int = {

    var maxLen = 0
    boxes.toList.foreach {
      box =>
        val strandLength = box.cells.length
        val cycleLength = if (box.cycleId != 0) {
          // Cycle through all cycles with this id throughout the
          // complete program run and fetch the longest one
          var longestCycleLen = 0
          this.boxSteps.boxSteps.foreach{
            boxStep =>
              if(boxStep.cycles.contains(box.cycleId)){
                val cycle = boxStep.cycles.get(box.cycleId).get
                if(cycle.cells.size > longestCycleLen){
                  longestCycleLen = cycle.cells.size
                }
              }
          }
          longestCycleLen
        } else 0
        if (strandLength + cycleLength > maxLen) {
          maxLen = strandLength + cycleLength
        }

    }
    maxLen

  }

  /**
   * Convert vertex to HTML
   *
   * @param innerNode the vertex to convert
   * @return the HTML
   */
  def nodeToHTML(innerNode: scalax.collection.Graph[DsOliMetaBoxGraphVertex, DsOliMetaBoxGraphDiEdge]#NodeT): String = {
    val funSignature = classSignature + "nodeToHTML: "
    val vertex = innerNode.value

    val tableStart = "<<TABLE><TR><TD COLSPAN=\"2\">" + innerNode.value + "</TD></TR><TR><TD COLSPAN=\"2\">"
    val tableEnd = "</TD></TR></TABLE>>"
    val ret = condOpt(vertex) {
      case ep: DsOliMetaBoxGraphVertexEP => tableStart + "ep: " + ep.ep.id + tableEnd
      case box: DsOliMetaBoxGraphVertexBoxes =>
        var ret = tableStart + box.boxType.vType + ":" + box.linkOffset + ": " +
          box.boxes.map(b => b.id) + " " + printNodeWithNestingRec(vertex, 0) + " maxStrandLength: " + getLongestStrandLength(box.boxes)

        //var nestingString = (vertex.nesting.foldLeft[String]("")((b, a) => b + ", " + a.name))
        //if (nestingString.length > 0) nestingString = " with nested " + nestingString.replaceFirst(", ", "")

        /*if (box.nesting.size > 0) {
          ret += nestingString
        }*/

        ret += "</TD></TR>"

        val inConConfs = new ListBuffer[DsOliConConf]()
        val outConConfs = new ListBuffer[DsOliConConf]()
        this.graph.graph.edges.iterator.foreach {
          edgeInner =>
            var portLine = "<TR>"
            var matched = false
            // Incoming port
            if (edgeInner.target == vertex) {

              val conConfExists = inConConfs.find(conConf => edgeInner.conConf.offsets._1 == conConf.offsets._1 && edgeInner.conConf.offsets._2 == conConf.offsets._2)
              if (!conConfExists.isDefined) {
                inConConfs += edgeInner.conConf
                portLine += "<TD PORT=\"" + edgeInner.conConf.offsets._1 + edgeInner.conConf.offsets._2 + "in\">" + edgeInner.conConf.offsets._1 + edgeInner.conConf.offsets._2 + "in</TD>"
                matched = true
              } else {
                portLine += "<TD></TD>"
              }
            } else {
              portLine += "<TD></TD>"
            }
            // Outgoing port
            if (edgeInner.source == vertex) {

              val conConfExists = outConConfs.find(conConf => edgeInner.conConf.offsets._1 == conConf.offsets._1 && edgeInner.conConf.offsets._2 == conConf.offsets._2)
              if (!conConfExists.isDefined) {
                outConConfs += edgeInner.conConf
                portLine += "<TD PORT=\"" + edgeInner.conConf.offsets._1 + edgeInner.conConf.offsets._2 + "out\">" + edgeInner.conConf.offsets._1 + edgeInner.conConf.offsets._2 + "out</TD>"
                matched = true
              } else {
                portLine += "<TD></TD>"
              }

            } else {
              portLine += "<TD></TD>"
            }
            if (matched)
              ret += portLine + "</TR>"
        }
        ret += "</TABLE>>" //tableEnd
        ret
      case vertex: DsOliMetaBoxGraphVertex =>
        var nestingString = (vertex.nesting.foldLeft[String]("")((b, a) => b + ", " + a.name))
        if (nestingString.length > 0) nestingString = " with nested " + nestingString.replaceFirst(", ", "")
        tableStart + vertex.name + nestingString + tableEnd
    }
    if (ret.isDefined) ret.get else "empty"
  }

  /**
   * Convert vertex to HTML unidirectional
   *
   * @param innerNode the vertex to convert
   * @return the HTML
   */
  def nodeToHTMLUnDi(innerNode: scalax.collection.Graph[DsOliMetaBoxGraphVertex, UnDiEdge]#NodeT): String = {
    val funSignature = classSignature + "nodeToHTML: "
    val vertex = innerNode.value

    val tableStart = "<<TABLE><TR><TD COLSPAN=\"2\">" + innerNode.value + "</TD></TR><TR><TD COLSPAN=\"2\">"
    val tableEnd = "</TD></TR></TABLE>>"
    val ret = condOpt(vertex) {
      case ep: DsOliMetaBoxGraphVertexEP => tableStart + "ep: " + ep.ep.id + " start: " + ep.start + " end: " + ep.end + " aggCnt: " + ep.aggCount + tableEnd
      case box: DsOliMetaBoxGraphVertexBoxes =>
        val fieldNameOpt = this.typeDB.getFieldNameAtOffset(box.linkOffset, box.boxType.vType)
        val fieldName = if (fieldNameOpt.isDefined) {
          fieldNameOpt.get
        } else {
          "unknown field name: " + box.linkOffset
        }
        var ret = tableStart + box.boxType.vType + ": field(@" + box.linkOffset + "): " + fieldName + ": " + box.boxes.map(b => b.id) + " " + printNodeWithNestingRec(vertex, 0)  + " maxStrandLength: " + getLongestStrandLength(box.boxes)
        ret += "</TD></TR>"
        ret += "</TABLE>>"
        ret
      case vertex: DsOliMetaBoxGraphVertex =>
        var nestingString = (vertex.nesting.foldLeft[String]("")((b, a) => b + ", " + a.name))
        if (nestingString.length > 0) nestingString = " with nested " + nestingString.replaceFirst(", ", "")
        tableStart + vertex.name + nestingString + tableEnd
    }
    if (ret.isDefined) ret.get else "empty"
  }
  /**
   * Convert connected vertex
   *
   * @param innerNode the vertex to convert
   * @return the DOT format
   */
  def cNodeTransformer(innerNode: scalax.collection.Graph[DsOliMetaBoxGraphVertex, DsOliMetaBoxGraphDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] = {
    Some((
      DotSubGraph(
        ancestor = dotHeader,
        subgraphId = "connected_vertices",
        kvList = Seq(DotAttr("rankdir", "LR"))),
        DotNodeStmt(innerNode.value.toString(), Seq(
          DotAttr("label", nodeToHTML(innerNode)),
          DotAttr("shape", "none")))))
  }

  /**
   * Convert connected vertex unidirectional
   *
   * @param innerNode the vertex to convert
   * @return the DOT format
   */
  def cNodeTransformerUnDi(innerNode: scalax.collection.Graph[DsOliMetaBoxGraphVertex, UnDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] = {
    Some((
      DotSubGraph(
        ancestor = dotHeader,
        subgraphId = "connected_vertices",
        kvList = Seq(DotAttr("rankdir", "LR"))),
        DotNodeStmt(innerNode.value.toString(), Seq(
          DotAttr("label", nodeToHTMLUnDi(innerNode)),
          DotAttr("shape", "none")))))
  }

  /**
   * Convert disconnected vertex
   *
   * @param innerNode the vertex to convert
   * @return the DOT format
   */
  def iNodeTransformer(innerNode: scalax.collection.Graph[DsOliMetaBoxGraphVertex, DsOliMetaBoxGraphDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] = {
    Some((
      DotSubGraph(
        ancestor = dotHeader,
        subgraphId = "disconnected_vertices",
        kvList = Seq.empty),
        DotNodeStmt(innerNode.value.toString(), Seq(
          DotAttr("label", nodeToHTML(innerNode)),
          DotAttr("shape", "none")))))
  }

  /**
   * Convert disconnected vertex unidirectional
   *
   * @param innerNode the vertex to convert
   * @return the DOT format
   */
  def iNodeTransformerUnDi(innerNode: scalax.collection.Graph[DsOliMetaBoxGraphVertex, UnDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] = {
    Some((
      DotSubGraph(
        ancestor = dotHeader,
        subgraphId = "disconnected_vertices",
        kvList = Seq.empty),
        DotNodeStmt(innerNode.value.toString(), Seq(
          DotAttr("label", nodeToHTMLUnDi(innerNode)),
          DotAttr("shape", "none")))))
  }

  /**
   * Convert graph into DOT output format
   *
   * @param graph the graph to convert
   * @param bxStps the current strand set
   * @return the graph representation in the DOT format
   */
  def createGraphOutput(graph: DsOliMetaBoxGraph, bxStps: DsOliBoxSteps): String = {
    val funSignature = classSignature + "createGraphOutput: "

    // Save the graph and the strand sets
    this.graph = graph
    this.boxSteps = bxStps
    this.doubleEdges.clear
    try {
      graph.graph.toDot(dotHeader, edgeTransformer, cNodeTransformer = Some(cNodeTransformer), iNodeTransformer = Some(iNodeTransformer))
    } catch {
      case e: Exception =>
        DsOliLogger.error(funSignature)
        throw e
    }
  }

  /**
   * Convert graph into DOT output format
   *
   * @param graph the graph to convert
   * @param tpDB the corresponding type DB
   * @param bxStps the current strand set
   * @return the graph representation in the DOT format
   */
  def createGraphOutputUnDi(graph: Graph[DsOliMetaBoxGraphVertex, UnDiEdge], tpDB: ITypeDB,
    bxStps: DsOliBoxSteps): String = {
    val funSignature = classSignature + "createGraphOutput: "

    // Save the typeDB and the strand sets
    this.typeDB = tpDB
    this.boxSteps = bxStps
    this.doubleEdges.clear
    try {
      graph.toDot(dotHeader, edgeTransformerUnDi, cNodeTransformer = Some(cNodeTransformerUnDi), iNodeTransformer = Some(iNodeTransformerUnDi))
    } catch {
      case e: Exception =>
        DsOliLogger.error(funSignature)
        throw e
    }
  }
}