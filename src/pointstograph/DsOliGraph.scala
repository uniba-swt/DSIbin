
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
 * DsOliGraph.scala created on Oct 9, 2014
 *
 * Description: A graph
 */
package pointstograph

import scalax.collection.mutable.Graph
import scala.collection.mutable.HashMap
import pointstograph.DsOliDiEdge._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import extlogger._
import scala.collection.mutable.ListBuffer
import binarytypes.DSIType
import PartialFunction._

class DsOliGraph(val ptgs: DsOliPointsToGraphs) {

  val classSignature = "DsOliGraph::"

  var graph = Graph[DsOliVertex, DsOliDiEdge]()

  // Keep track of the last used node / edge
  var lastEdge: DsOliDiEdge[DsOliVertex] = null
  var lastNode: DsOliVertex = null

  // Shortcut for mapping an address to a vertex
  val addressToVertex = new HashMap[Long, DsOliVertex]

  // Storage for graphs created by artificial events
  var artificialGraphs = new ListBuffer[DsOliGraph]()

  /**
   * Add a new edge to the graph
   *
   * @param edge the edge to add
   * @return Boolean if the edge was added
   */
  def add(edge: DsOliDiEdge[DsOliVertex]): Boolean = {
    val funSignature = classSignature + "add: "
    val retVal = graph.add(edge)
    if (retVal) {
      // Keep track of the last added edge
      lastEdge = edge
      val sourceId = edge.source.id
      val targetId = edge.target.id
      ptgs.idToNode.put(sourceId, edge.source)
      ptgs.idToNode.put(targetId, edge.target)
    }
    return retVal
  }

  /**
   * Add a new node to the graph
   *
   * @param node the vertex to add
   * @return Boolean if the node was added
   */
  def add(node: DsOliVertex): Boolean = {
    val retVal = graph.add(node)
    if (retVal) {
      // Keep track of the last added node
      lastNode = node
      ptgs.idToNode.put(node.id, node)
    }
    return retVal
  }

  /**
   * Fetch a vertex by its id
   *
   * @param id the id of the vertex
   * @return Option the vertex
   */
  def get(id: Long): Option[DsOliVertex] = {
    val node = if (ptgs.idToNode.contains(id)) {
      Some(graph.get(ptgs.idToNode.get(id).get).value)
    } else {
      None
    }
    return node
  }

  /**
   * Do a deep copy of the graph
   */
  def deepCopy(): DsOliGraph = {
    val funSignature = classSignature + "deepCopy: "
    var newGraph = new DsOliGraph(ptgs)

    // Copy the different nodes
    DsOliLogger.debug(funSignature + "nodes")
    val iter = graph.nodes.iterator
    while (iter.hasNext) {
      val next = iter.next.value
      val curNode = next match {
        // Predefined vertices are reused
        case n: DsOliVertexPredefined =>
          DsOliLogger.debug(funSignature + "Predefined vertex: " + n)
          n
        // Create new type vertex for merged type graph
        case n: DsOliTypeVertex =>
          DsOliLogger.debug(funSignature + "DsOliTypeVertex: " + n)
          // Important, copy over all the collected type instances
          val newInstances = new ListBuffer[(DSIType, Long)]
          n.dsiTypeInstances.foreach {
            typeInstance =>
              val (instance, instanceOffset) = typeInstance
              newInstances.append((instance.deepCopy, instanceOffset))
          }
          new DsOliTypeVertex(n.bAddr, n.eAddr, n.vType, n.id, n.isStack,
            n.typeID, n.offsetFromStart, newInstances)
        // Create new memory vertex
        case n: DsOliVertexMemory =>
          DsOliLogger.debug(funSignature + "Memory vertex: " + n)
          new DsOliVertexMemory(n.bAddr, n.eAddr, n.vType, n.id, n.isStack)
        case n =>
          DsOliLogger.debug(funSignature + "Default: " + n);
          n
      }

      newGraph.graph.add(curNode)
      newGraph.ptgs.idToNode.put(next.id, curNode)
    }

    // Copy the edges
    DsOliLogger.debug(funSignature + "edges")
    val edgeIter = graph.edges.iterator
    while (edgeIter.hasNext) {
      val next = edgeIter.next.edge
      val source = next.source.value
      val target = next.target.value
      DsOliLogger.debug(funSignature + "source " + source + "," + source.id
        + "; target: " + target + "," + target.id)

      if (newGraph.ptgs.idToNode.contains(source.id) &&
        newGraph.ptgs.idToNode.contains(target.id)) {
        val sourceVertex = newGraph.ptgs.idToNode.get(source.id).get
        val targetVertex = newGraph.ptgs.idToNode.get(target.id).get
        val edge = sourceVertex ~> targetVertex toDsOliDiEdge (next.id, next.sAddr,
          next.sOffset, next.tAddr, next.tOffset)
        newGraph.graph.add(edge)
      }

    }
    return newGraph
  }

  /**
   * Calculate the current target address of a given source address,
   * i.e, find the edge for the given source address where there
   * can exist only one.
   *
   * @param sAddr the source address to search for
   * @return Option of the corresponding target address
   */
  def getCurrentTargetAddr(sAddr: Long): Option[Long] = {

    val funSignature = classSignature + "getCurrentTargetAddr: "
    DsOliLogger.debug(funSignature + "searching for sAddr: " + sAddr.toHexString)

    // Fetch the vertex for the source address or return
    val sourceVertexOpt = this.getVertexForAddress(sAddr)
    if (sourceVertexOpt.isEmpty) return None
    val sourceVertex = sourceVertexOpt.get

    // Fetch the offset or return
    val sOffsetOpt = condOpt(sourceVertex) { case s: DsOliVertexMemory => sAddr - s.bAddr }
    if (sOffsetOpt.isEmpty) return None

    val sOffset = sOffsetOpt.get
    DsOliLogger.debug(funSignature + "sOffset: " + sOffset.toHexString)

    // Iterate through all edges to fetch the target address
    val edgeIter = this.graph.edges.iterator
    while (edgeIter.hasNext) {
      val edge = edgeIter.next.edge

      DsOliLogger.debug(funSignature + "testing against " + edge.sAddr.toHexString
        + " && edge.sOffset " + edge.sOffset.toHexString)
      if (edge.sAddr + edge.sOffset == sAddr) {
        DsOliLogger.debug(funSignature + "found edge and tAddr: " +
          (edge.tAddr + edge.tOffset).toHexString)
        return Some(edge.tAddr + edge.tOffset)
      }
    }

    DsOliLogger.debug(funSignature + "no edge found")
    return None
  }

  /**
   * Helper to cast from vertex to memory vertex
   *
   * @param vertex the vertex to cast
   * @returns instance of a memory vertex
   */
  def getMemoryVertex(vertex: DsOliVertex): DsOliVertexMemory = {
    vertex match {
      case v: DsOliVertexMemory => v
      case _ => throw new Exception("Not a memory vertex")
    }
  }

  /**
   * Search for the corresponding vertex for a given address
   *
   * @param address the address to search for
   * @returns instance of the vertex containing the address
   */
  def getVertexForAddress(address: Long): Option[DsOliVertex] = {
    val funSignature = classSignature + "getVertexForAddress: "
    DsOliLogger.debug(funSignature + "Searching for address " + address.toHexString)

    // Shortcut if address to vertex mapping was already done
    if (addressToVertex.contains(address)) return addressToVertex.get(address)

    // Iterate through all nodes
    val iter = graph.nodes.iterator
    while (iter.hasNext) {
      val next = iter.next.value
      next match {
        case n: DsOliVertexMemory =>
          DsOliLogger.debug(funSignature + "vertex start/end: " + n.bAddr.toHexString
            + "," + n.eAddr.toHexString)
          if (n.bAddr <= address && address <= n.eAddr) {
            DsOliLogger.debug(funSignature + "in range " + n)
            addressToVertex.put(address, n)
            return Some(n)
          } else {
            DsOliLogger.debug(funSignature + "not in range " + n)
          }
        case _ =>
          DsOliLogger.debug(funSignature + "Something else " + next.value)
      }
    }
    return None
  }
}

object DsOliGraph {
  // These nodes are common for all graphs: NULL and undefined
  val vertexNull = new DsOliVertexNull(0)
  val vertexUndef = new DsOliVertexUndef(1)
}