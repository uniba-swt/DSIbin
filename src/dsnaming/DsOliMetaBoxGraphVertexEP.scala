
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
 * DsOliMetaBoxVertexEP.scala created on Jun 19, 2015
 *
 * Description: The entry pointer vertex representation
 * inside of a strand graph
 *
 */
package dsnaming

import pointstograph.DsOliVertex

/**
 * @author DSI
 *
 * @constructor creates entry pointer vertex for the strand graph
 * @param ep the points-to graph entry pointer vertex
 * @param start the start time step of the entry pointer
 * @param end the end time step of the entry pointer
 * @param aggCount the number of aggregations for the entry pointer
 */
class DsOliMetaBoxGraphVertexEP(val ep: DsOliVertex, var start: Long, var end: Long, var aggCount: Long) extends DsOliMetaBoxGraphVertex {
  def this(ep: DsOliVertex) = this(ep, -1, -1, -1)
}