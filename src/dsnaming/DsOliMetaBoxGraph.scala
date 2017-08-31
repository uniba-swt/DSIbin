
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
 * DsOliMetaBoxGraph.scala created on Jun 19, 2015
 *
 * Description: The representation of a strand graph
 */
package dsnaming

import scalax.collection.mutable.Graph
import pointstograph.DsOliVertex
import PartialFunction._
import boxcalculation.DsOliBox
import extlogger.DsOliLogger

/**
 * @author DSI
 *
 */
class DsOliMetaBoxGraph {
  val classSignature = "DsOliMetaBoxGraph::"

  // The strand graph
  val graph = Graph[DsOliMetaBoxGraphVertex, DsOliMetaBoxGraphDiEdge]()

  /**
   * Checks, if the given strand vertex is an entry pointer
   * vertex in the strand graph and that the associated
   * entry pointer of the points-to graph matches the
   * given entry pointer of the points-to graph.
   *
   * @param mbgVertex the strand vertex of the strands graph
   * @param ep the entry pointer vertex of the points-to graph
   * @return Boolean
   */
  def isEqualMBGVertexWithEP(mbgVertex: DsOliMetaBoxGraphVertex, ep: DsOliVertex): Boolean = {
    mbgVertex.isInstanceOf[DsOliMetaBoxGraphVertexEP] && mbgVertex.asInstanceOf[DsOliMetaBoxGraphVertexEP].ep == ep
  }
  /**
   * Checks if entry pointer is already present
   *
   * @param ep the entry pointer vertex of the points-to graph to search for
   * @return Boolean to indicate if the EP is already present
   */
  def hasEPVertex(ep: DsOliVertex): Boolean = {
    graph.nodes.exists(node => isEqualMBGVertexWithEP(node.value, ep))
  }

  /**
   * Add an entry pointer vertex of the points-to graph 
   * into the strand graph
   *
   * @param ep the entry pointer vertex of the points-to graph to add
   * @return Boolean to indicate if element was added
   */
  def addEPVertex(ep: DsOliVertex): Boolean = {
    hasEPVertex(ep) || graph.add(new DsOliMetaBoxGraphVertexEP(ep))
  }

  /**
   * Add an entry pointer vertex of the points-to graph 
   * into the strand graph
   * 
   * @param ep the entry pointer vertex of the points-to graph to add
   * @param start the start time step of the entry pointer
   * @param end the start time step of the entry pointer
   * @param aggCnt the aggregation count for the vertex
   * @return Boolean to indicate if element was added
   */
  def addEPVertex(ep: DsOliVertex, start: Long, end: Long, aggCnt: Long): Boolean = {
    hasEPVertex(ep) || graph.add(new DsOliMetaBoxGraphVertexEP(ep, start, end, aggCnt))
  }

  /**
   * Try to find the vertex equal to the
   * partially applied isEqual method passed
   * into getVertex
   * 
   * @param isEqual partially applied function to find the vertex
   * @return Option the found vertex
   * 
   */
  def getVertex(isEqual: DsOliMetaBoxGraphVertex => Boolean): Option[DsOliMetaBoxGraphVertex] = {
    val optVertex = graph.nodes.find(node => isEqual(node.value))
    if (optVertex.isDefined) Some(optVertex.get.value) else None
  }

  /**
   * Get the corresponding entry pointer vertex of the strand graph,
   * from the given points-to graph vertex.
   * 
   * @param ep the entry pointer from the points-to graph
   * @return Option the corresponding vertex from the strand graph
   */
  def getEPVertex(ep: DsOliVertex): Option[DsOliMetaBoxGraphVertex] = {
    getVertex(isEqualMBGVertexWithEP(_, ep))
  }

  /**
   * Checks, if the strand vertex contains all strands
   * of the given strand set.
   * 
   * @param mbgVertex the strand vertex
   * @param boxes the set of strands
   * @return Boolean
   */
  def isEqualMBGVertexWithSet(mbgVertex: DsOliMetaBoxGraphVertex, boxes: Set[DsOliBox]): Boolean = {
    if (mbgVertex.isInstanceOf[DsOliMetaBoxGraphVertexBoxes]) {
      val boxMbgVertex = mbgVertex.asInstanceOf[DsOliMetaBoxGraphVertexBoxes]
      // The set needs to be matched exactly, i.e., check for size and that 
      // all elements are found
      val size = boxMbgVertex.boxes.size == boxes.size
      val equalBoxes = boxes.forall(box => boxMbgVertex.boxes.exists(boxPrime => box.id == boxPrime.id))
      size && equalBoxes
    } else {
      false
    }
  }

  /**
   * Checks, if there exists a vertex with the given strand set
   * 
   * @param boxes the strand set
   * @return Boolean
   */
  def hasBoxesVertex(boxes: Set[DsOliBox]): Boolean = {
    graph.nodes.exists(node => isEqualMBGVertexWithSet(node.value, boxes))
  }

  /**
   * Create a vertex with the given strand set
   * 
   * @param boxes the strand set
   * @return Boolean
   */
  def addBoxesVertex(boxes: Set[DsOliBox]): Boolean = {
    hasBoxesVertex(boxes) || graph.add(new DsOliMetaBoxGraphVertexBoxes(boxes))
  }

  /**
   * Get the vertex which corresponds to the given strand set
   * 
   * @param boxes the set of strands to search for
   * @return Option the corresponding vertex
   */
  def getBoxesVertex(boxes: Set[DsOliBox]): Option[DsOliMetaBoxGraphVertex] = {
    getVertex(isEqualMBGVertexWithSet(_, boxes))
  }

  /**
   * Get the vertex which contains exactly the given strand
   *  
   * @param box the strand to search for
   * @return Option the corresponding vertex
   */
  def getBoxVertex(box: DsOliBox): Option[DsOliMetaBoxGraphVertex] = {
    getBoxesVertex(Set[DsOliBox](box))
  }

  /**
   * toString helper producing a string representation of a vertex
   * 
   * @param n the vertex to print
   * @param prefix the prefix for the produced string
   * @param suffix the suffix for the produced string
   * @return the string representation of the vertex
   */
  def printNode(n: DsOliMetaBoxGraphVertex, prefix: String = "\t", suffix: String = "\n"): String = {
    prefix +
      (n match {
        case ep: DsOliMetaBoxGraphVertexEP => "ep: " + ep.ep.id + ";"
        case b: DsOliMetaBoxGraphVertexBoxes => "box: " + b.boxes.map(bb => bb.id) + ";"
        case _ => ""
      }) + suffix
  }

  override def toString(): String = {
    var ret = "MBG: \n"
    this.graph.nodes.foreach { n =>
      ret += printNode(n.value, suffix = "") + "(outer:" + n.value + " :: inner:" + n + ")\n"
    }
    ret += "\n"
    this.graph.edges.foreach {
      e =>
        val edge = e.toOuter
        ret += "\tedge: " + printNode(edge.source, prefix = "", suffix = "") + " -> " +
          printNode(edge.target, prefix = "", suffix = "") + "(" + edge + ")\n"
    }
    ret
  }

  /**
   * Get the corresponding entry pointer of strand set vertex
   * 
   * @param vertex the vertex to search for
   * @return Option the corresponding strand vertex
   */
  def getVertex(vertex: DsOliMetaBoxGraphVertex): Option[DsOliMetaBoxGraphVertex] = {
    vertex match {
      case vertex: DsOliMetaBoxGraphVertexEP => getEPVertex(vertex.ep)
      case vertex: DsOliMetaBoxGraphVertexBoxes => getBoxesVertex(vertex.boxes)
      case _ => None
    }
  }

  /**
   * Deep copy of strand graph
   * 
   * @return a copy of the current graph
   */
  def deepCopy(): DsOliMetaBoxGraph = {
    val funSignature = classSignature + "deepCopy: "

    // Create the vertices
    val newMbg = new DsOliMetaBoxGraph
    this.graph.nodes.foreach { n =>
      newMbg.graph.add(n.value match {
        case ep: DsOliMetaBoxGraphVertexEP => new DsOliMetaBoxGraphVertexEP(ep.ep)
        case b: DsOliMetaBoxGraphVertexBoxes => new DsOliMetaBoxGraphVertexBoxes(b.boxes)
      })
    }
    
    // Create the edges
    this.graph.edges.foreach {
      e =>
        val sourceOpt = newMbg.getVertex(e.source)
        val targetOpt = newMbg.getVertex(e.target)
        val newEdge = new DsOliMetaBoxGraphDiEdge[DsOliMetaBoxGraphVertex](sourceOpt.get, targetOpt.get, e.ccSet, e.conConf, e.conConfClass)
        newEdge.ccSetPerTimeStep = e.ccSetPerTimeStep
        newMbg.graph.add(newEdge)
    }
    DsOliLogger.debug(funSignature + "copied graph: " + newMbg)
    newMbg
  }
}