
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
 * DsOliOffsGroups.scala created on Jun 19, 2015
 *
 * Description: Store the connection configurations and the
 * set of associated cells and strands
 */
package dsnaming

import boxcalculation.DsOliCell
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import boxcalculation.DsOliBox

/**
 * @author DSI
 *
 */
class DsOliOffsGroups() {

  // Key: connection configuration -> Value: set of cells and strands for this connection configuration
  val conConfToCells = new ListBuffer[(DsOliConConf, Set[(DsOliCell, DsOliCell, DsOliBox, DsOliBox)])]

  /**
   * Add a connection configuration
   *
   * @param conConf the connection configuration
   * @param cellSet the set of cells and strands for this connection configuration
   * @return Boolean indicating if it was possible to add the connection configuration
   */
  def addCC(conConf: DsOliConConf, cellSet: (DsOliCell, DsOliCell, DsOliBox, DsOliBox)): Boolean = {
    val conConfEntry = conConfToCells.filter { case (cc, _) => cc == conConf }
    // If connection configuration not present yet, create new else append
    if (conConfEntry.size == 0) {
      conConfToCells.append((conConf, Set[(DsOliCell, DsOliCell, DsOliBox, DsOliBox)](cellSet)))
      true
    } else {
      conConfEntry.head._2.add(cellSet)
    }
  }

}