
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
 * VertexObject.scala created on Oct 6, 2014
 *
 * Description: A memory vertex
 */
package pointstograph

/**
 * @author DSI
 *
 */
/**
 * @constructor create a memory vertex
 * @param bAddr the start address of the vertex
 * @param eAddr the end address of the vertex
 * @param vType the type associated with the vertex
 * @param id a unique id for this vertex
 * @param isStack indicates where the vertex resides (stack or heap)
 */
class DsOliVertexMemory(
  var bAddr: Long,
  var eAddr: Long,
  val vType: DsOliType,
  val id: Long,
  val isStack: Boolean = false) extends DsOliVertex {

  override def toString(): String = {
    "[" + "bAddr = " + bAddr.toHexString + "," + " eAddr = " +
      eAddr.toHexString + "," + " vType = " + vType + "," +
      " id = " + id + ", isStack = " + isStack + "]"
  }

}