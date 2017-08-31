
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
 * DsOliGraphUtils.scala created on Nov 20, 2015
 *
 * Description: Helper for functions for dealing with Graphs, e.g., finding 
 * bidirectional edges.
 */
package util

import dsnaming.DsOliMetaBoxGraphDiEdge
import dsnaming.DsOliMetaBoxGraphVertex
import dsnaming.DsOliMetaBoxGraph
import extlogger.DsOliLogger
import dsnaming.DsOliMetaBoxGraphVertexBoxes
import dsnaming.DsOliMetaBoxGraphVertexEP
import dsnaming.DsOliConConfClassification
import scala.collection.mutable.HashMap
import dsnaming.DsOliConConfClassificationTag._
import dsnaming.DsOliConConfClassificationTag
import dsnaming.DsOliConConfTag._

/**
 * @author DSI
 *
 */
object DsOliGraphUtils {

  val classSignature = "DsOliGraphUtils::"

  /**
   * Find the corresponding edge for a bidirectional edge.
   * Bidirectional edges are represented with two directed edges
   * running in reverse.
   *
   * @param edgeBiDi the edge to find the corresponding edge for
   * @param graph the graph to search in
   * @return the corresponding edge
   */
  def getCorrespondingBiDiEdge(edgeBiDi: DsOliMetaBoxGraphDiEdge[DsOliMetaBoxGraphVertex], graph: DsOliMetaBoxGraph): DsOliMetaBoxGraphDiEdge[DsOliMetaBoxGraphVertex] = {
    val funSignature = classSignature + "gtCrrspndngBDdg: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Must be overlay AND there must exist a corresponding edge where source and target are swapped and the 
    // swapped offsets are matched
    val biDiCounterpartSet = graph.graph.edges.filter { edgeBiDiCounter =>
      edgeBiDiCounter.conConf.tag == ccOverlay &&
        edgeBiDi.source == edgeBiDiCounter.toOuter.target && edgeBiDi.target == edgeBiDiCounter.toOuter.source &&
        edgeBiDiCounter.conConf.offsets._1 == edgeBiDi.conConf.offsets._2 &&
        edgeBiDiCounter.conConf.offsets._2 == edgeBiDi.conConf.offsets._1
    }

    // Sanity check
    if (biDiCounterpartSet.size == 1) {
      biDiCounterpartSet.head.toOuter
    } else {
      biDiCounterpartSet.foreach {
        biDi =>
          println(funSignature + "found bi di alternative: " + biDi.toOuter)
      }
      throw new Exception(funSignature + "No bi directional counterpart found!: " + biDiCounterpartSet.size + "\nedge: " + edgeBiDi)
    }
  }

  /**
   * Checks, if the given edge is a bidirectional edge.
   * Bidirectional edges are represented by two directed edges, so
   * the there must be a corresponding edge where the connection tag
   * must be overlay (only overlay is bidirectional) and the
   * connection configuration offsets are matched swapped wise.
   *
   * @param edge the edge to find a corresponding edge for
   * @param graph the graph to search in
   * @return Boolean
   */
  def isBidirectionalEdge(edge: DsOliMetaBoxGraphDiEdge[DsOliMetaBoxGraphVertex], graph: DsOliMetaBoxGraph): Boolean = {
    val funSignature = classSignature + "sBdrctnldg: "
    DsOliLogger.debug(funSignature + "entered: ")
    // Must be overlay AND there must exist a corresponding edge where source and target are swapped and the 
    // swapped offsets are matched
    edge.conConf.tag == ccOverlay && graph.graph.edges.exists(edgeBiDi => edgeBiDi.conConf.tag == edge.conConf.tag &&
      edgeBiDi.source == edge.target && edgeBiDi.target == edge.source &&
      edge.conConf.offsets._1 == edgeBiDi.conConf.offsets._2 &&
      edge.conConf.offsets._2 == edgeBiDi.conConf.offsets._1)
  }

  def findCorrespondingEdge(edge: DsOliMetaBoxGraphDiEdge[DsOliMetaBoxGraphVertex], dsOliMbg: DsOliMetaBoxGraph): Option[dsOliMbg.graph.EdgeT] = {
    //    val funSignature = classSignature + "findCorrespondingEdge: "
    val funSignature = classSignature + "fndCrrspndngdg: "
    DsOliLogger.debug(funSignature + "entered: ")
    dsOliMbg.graph.edges.find {
      edgeInnerMain =>
        val edgeMain = edgeInnerMain.toOuter
        val (source, target) = sourceTargetStrHelper(edgeMain)
        DsOliLogger.debug(funSignature + "processing edge: " + source + "->" + target)
        if (edgeMain.source.isInstanceOf[DsOliMetaBoxGraphVertexBoxes] && edgeMain.target.isInstanceOf[DsOliMetaBoxGraphVertexBoxes]) {
          val sourceMain = edgeMain.source.asInstanceOf[DsOliMetaBoxGraphVertexBoxes]
          val targetMain = edgeMain.target.asInstanceOf[DsOliMetaBoxGraphVertexBoxes]
          val source = edge.source.asInstanceOf[DsOliMetaBoxGraphVertexBoxes]
          val target = edge.target.asInstanceOf[DsOliMetaBoxGraphVertexBoxes]
          DsOliLogger.debug(funSignature + "sourceMain: " + sourceMain.boxes)
          DsOliLogger.debug(funSignature + "targetMain: " + targetMain.boxes)
          DsOliLogger.debug(funSignature + "source: " + source.boxes)
          DsOliLogger.debug(funSignature + "target: " + target.boxes)
          if (source.boxes.head.id == sourceMain.boxes.head.id &&
            target.boxes.head.id == targetMain.boxes.head.id &&
            //            edge.conConf.offsets._1 == edgeMain.conConf.offsets._1 &&
            //            edge.conConf.offsets._2 == edgeMain.conConf.offsets._2) {
            edge.conConf == edgeMain.conConf) {
            DsOliLogger.debug(funSignature + "is corresponding edge")
            true
          } else {
            DsOliLogger.debug(funSignature + "is no corresponding edge")
            false
          }
        } else {
          DsOliLogger.debug(funSignature + "not two box vertices")
          false
        }
    }
  }

  /**
   * For debugging. Get a source and target string representation of the given edge
   *
   * @param edgeOuter the edge for which source and target will be transformed into strings
   * @return tuple with a string for source and target vertex
   */
  def sourceTargetStrHelper(edgeOuter: DsOliMetaBoxGraphDiEdge[DsOliMetaBoxGraphVertex]): (String, String) = {
    val funSignature = classSignature + "srcTrgtStrHlpr: "
    DsOliLogger.debug(funSignature + "entered: ")
    val sourceStr = if (edgeOuter.source.isInstanceOf[DsOliMetaBoxGraphVertexBoxes]) {
      edgeOuter.source.asInstanceOf[DsOliMetaBoxGraphVertexBoxes].boxes.head.id.toString
    } else if (edgeOuter.source.isInstanceOf[DsOliMetaBoxGraphVertexEP]) {
      "ep:" + edgeOuter.source.asInstanceOf[DsOliMetaBoxGraphVertexEP].ep.id.toString
    } else {
      "unsupported instance"
    }

    val targetStr = if (edgeOuter.target.isInstanceOf[DsOliMetaBoxGraphVertexBoxes]) {
      edgeOuter.target.asInstanceOf[DsOliMetaBoxGraphVertexBoxes].boxes.head.id.toString
    } else if (edgeOuter.target.isInstanceOf[DsOliMetaBoxGraphVertexEP]) {
      "ep:" + edgeOuter.target.asInstanceOf[DsOliMetaBoxGraphVertexEP].ep.id.toString
    } else {
      "unsupported instance"
    }

    (sourceStr, targetStr)
  }

  /**
   *  Fetch the label with the highest evidence count.
   *
   *  @param ccCount the label with the corresponding label and evidence count
   *  @return the labels descending on the evidence count
   */
  def highestConConfClass(ccCount: HashMap[DsOliConConfClassificationTag, (Int, Int)]): List[DsOliConConfClassificationTag] = {
    ccCount.keys.toList.sortBy(f => ccCount.get(f).get._2).reverse
  }

  /**
   * Fetch the classification counts
   *
   * @param conConfClass set of connection configuration classifications
   * @return Hash Map with the classification label and the corresponding label and evidence counts
   */
  def conConfClassAgg(conConfClass: scala.collection.mutable.Set[DsOliConConfClassification]): HashMap[DsOliConConfClassificationTag, (Int, Int)] = {
    val ccCount = new HashMap[DsOliConConfClassificationTag, (Int, Int)]()
    conConfClass.foreach { ccClass =>
      if (ccCount.contains(ccClass.classification)) {
        ccCount.put(ccClass.classification, (ccCount.get(ccClass.classification).get._1 + 1, ccCount.get(ccClass.classification).get._2 + ccClass.evidence))
      } else {
        ccCount.put(ccClass.classification, (1, ccClass.evidence))
      }
    }
    ccCount
  }

  /**
   * Color codes for the detected data structures used for DOT
   * @param key the classification
   * @return the color code string
   */
  def getColorForClass(key: DsOliConConfClassificationTag): String = {
    key match {
      // Intersecting group 
      case DsOliConConfClassificationTag.I1o => "#00FFCC"
      case DsOliConConfClassificationTag.I2o => "#00FF33"
      case DsOliConConfClassificationTag.SHN => "#99CC00"
      case DsOliConConfClassificationTag.I1i => "#99FF00"
      // Nesting group
      case DsOliConConfClassificationTag.No => "#3366FF"
      case DsOliConConfClassificationTag.Ni => "#9999FF"
      // Tree
      case DsOliConConfClassificationTag.BT => "#66CCFF"
      // Skip List
      case DsOliConConfClassificationTag.SLo2 => "#FFFF99"
      // DLL
      case DsOliConConfClassificationTag.DLL => "#993333"
      case _ => "#CC9900"
    }

  }

  /**
   * Create a compact representation of the labels and evidence counts:
   * descending list of labels sorted on the corresponding evidence count
   * 
   * @param conConfClass the classification to compact
   * @return the string representation
   */
  def compactCCClass(conConfClass: scala.collection.mutable.Set[DsOliConConfClassification]): String = {
    var ret = ""
    if (conConfClass != null) {
      val ccCount = conConfClassAgg(conConfClass)

      val sortedccCount = highestConConfClass(ccCount)
      sortedccCount.foreach {
        key =>
          val (cnt, evidence) = ccCount.get(key).get
          val color = "BGCOLOR=\"" + getColorForClass(key) + "\""
          ret += "<TR><TD ALIGN=\"left\" " + color + ">" + key + "</TD><TD ALIGN=\"left\" " + color + ">" + evidence + " (" + cnt + ")</TD></TR>"
      }
    }
    ret
  }

}