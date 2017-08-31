
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
 * PointsToGraphs.scala created on Oct 6, 2014
 *
 * Description: Stores all points-to graphs of
 * the event trace
 */
package pointstograph

import scala.collection.mutable.ListBuffer
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scala.collection.mutable.HashMap

/**
 * @author DSI
 *
 */
class DsOliPointsToGraphs {

  // List of all points-to graphs
  val graphs: ListBuffer[DsOliGraph] = new ListBuffer[DsOliGraph]

  // Storage of all nodes in a single place
  val idToNode = new HashMap[Long, DsOliVertex]()

  /**
   * Add a new graph to the end of the list of graphs
   * 
   * @param graph the graph to add
   */
  def append(graph: DsOliGraph): Unit = {
    graphs.append(graph)
  }

  /**
   * Get a graph by its index in the graph list
   * 
   * @param index the index to fetch
   * @return return the graph
   */
  def get(index: Int): DsOliGraph = {
    return graphs(index)
  }

  /**
   * Put the graph at the given index of the list of graphs
   * 
   * @param index the index to store the graph at
   * @param graph the graph to store
   */
  def put(index: Int, graph: DsOliGraph): Unit = {
    graphs(index) = graph
  }
  
  /**
   * Get a deep copy of the graph at the given index
   * 
   * @param index the index to retrieve a deep copy from
   * @return A deep copy of the graph at the given index
   */
  def getCopyFromStep(index : Int) : DsOliGraph = {
    return graphs(index).deepCopy
  }
}