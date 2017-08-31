
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
 * Edge.scala created on Oct 6, 2014
 *
 * Description: A directed edge
 */
package pointstograph

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphEdge.ExtendedKey
import scalax.collection.GraphEdge.EdgeCopy
import scalax.collection.GraphPredef.OuterEdge
import scalax.collection.GraphEdge.NodeProduct
import pointstograph.DsOliDiEdge._

/**
 * @author DSI
 *
 */
/**
 * @constructor create a directed edge with the given type
 * @param id unique id for an edge
 * @param sAddr the source address of the vertex where the edge originates
 * @param sOffset the source offset of the edge
 * @param tAddr the target address of the vertex where the edge originates
 * @param tOffset the target address of the edge
 * @param argCodeFragment the argument passed to free
 * @param argValue the size of the free
 */
class DsOliDiEdge[N](
  nodes: Product,
  val id: EdgeId,
  val sAddr: Long,
  val sOffset: Long,
  val tAddr: Long,
  val tOffset: Long) extends DiEdge[N](nodes)
  with ExtendedKey[N]
  with EdgeCopy[DsOliDiEdge]
  with OuterEdge[N, DsOliDiEdge] {

  def this(source: DsOliVertex, target: DsOliVertex, sAddr: Long,
    sOffset: Int,
    tAddr: Long,
    tOffset: Int) =
    this(NodeProduct(source, target), DsOliDiEdge.getId, sAddr, sOffset, tAddr, tOffset)

  // As we reuse the edges in the merged type graph, we now need to be able to have
  // multiple outgoing edges from one offset
  def keyAttributes = Seq(id)

  override def copy[NN](newNodes: Product) =
    new DsOliDiEdge[NN](newNodes, id, sAddr, sOffset, tAddr, tOffset)

}

object DsOliDiEdge {
  
  def apply(source: DsOliVertex, target: DsOliVertex, sAddr: Long,
    sOffset: Int,
    tAddr: Long,
    tOffset: Int) =
    new DsOliDiEdge[DsOliVertex](NodeProduct(source, target), id, sAddr, sOffset, tAddr, tOffset)

  def unapply(e: DsOliDiEdge[DsOliVertex]): Option[(DsOliVertex, DsOliVertex, Long, Long, Long, Long)] =
    if (e eq null) None else Some(e.from, e.to, e.sAddr, e.sOffset, e.tAddr, e.tOffset)

  // ! Do an upcast here to DsOliVertex
  implicit class DsOliEdgeAssoc[V <: DsOliVertex](val edge: DiEdge[V]) {
    @inline def toDsOliDiEdge(sAddr: Long, sOffset: Long, tAddr: Long, tOffset: Long) =
      new DsOliDiEdge[DsOliVertex](edge.nodes, DsOliDiEdge.getId, sAddr, sOffset, tAddr, tOffset) with OuterEdge[DsOliVertex, DsOliDiEdge]
    @inline def toDsOliDiEdge(id: EdgeId, sAddr: Long, sOffset: Long, tAddr: Long, tOffset: Long) =
      new DsOliDiEdge[DsOliVertex](edge.nodes, id, sAddr, sOffset, tAddr, tOffset) with OuterEdge[DsOliVertex, DsOliDiEdge]
  }

  // EdgeId typedef
  type EdgeId = Long

  // Important to start from 1, as zero means 0 reference
  var id: EdgeId = 1

  /**
   * Simply edge id generator
   * 
   * @returns a unique edge id
   */
  def getId(): Long = {
    val retId = id;
    id += 1
    return retId
  }
}

