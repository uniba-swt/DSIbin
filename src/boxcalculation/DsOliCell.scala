
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
 * DsOliCell.scala created on Oct 29, 2014
 *
 * Description: Representation of a cell, i.e.,
 * subregion of memory
 */
package boxcalculation

import pointstograph.DsOliVertexMemory
import boxcalculation.DsOliCell._
import boxcalculation.DsOliBox._
import scala.collection.mutable.Set
import pointstograph.DsOliType
import extlogger.DsOliLogger

/**
 * @author DSI
 *
 * @constructor creates a cell
 * @param id unique cell id
 * @param bAddr the start address of this cell in memory
 * @param eAddr the end address of this cell in memory
 * @param cType the type associated with this cell
 * @param vertexId the vertex id to which this cell belongs
 */
class DsOliCell(val id: CellId, val bAddr: Long, val eAddr: Long, val cType: DsOliType, var vertexId: Long) {

  // Convenience constructors
  def this(bAddr: Long, eAddr: Long, cType: DsOliType, vertexId: Long) = this(DsOliCell.getId(), bAddr, eAddr, cType, vertexId)

  /**
   * Create a deep copy of this cell, by carrying over
   * the id for the cell
   *
   * @param boxStep the time step for the strand
   * @return instance of the copied cycle
   */
  def deepCopy(): DsOliCell = {
    // Important to carry over the id
    val copy = new DsOliCell(this.id, this.bAddr, this.eAddr, this.cType, this.vertexId)
    return copy
  }

  override def toString(): String = {
    "[" + this.getClass() + ": id = " + id + "," + "bAddr = " + bAddr.toHexString + "," + "eAddr = " + eAddr.toHexString + "," + "cType = " + cType + "," + "vertexRef = " + vertexId + "]"
  }

  override def equals(other: Any): Boolean = {
    return other match {
      case that: DsOliCell =>
        if (that == null) {
          DsOliLogger.error("DsOliCell::equals: called with NULL!")
          return false
        }
        DsOliLogger.debug("DsOliCell::equals: called on element " + id + " with " + that.id)
        DsOliLogger.debug("DsOliCell::equals: (this == that): " +
          this.bAddr.toHexString + " == " + that.bAddr.toHexString + " && " +
          this.eAddr.toHexString + " == " + that.eAddr.toHexString + " && " +
          this.cType + " == " + that.cType)
        this.bAddr == that.bAddr && this.eAddr == that.eAddr && this.cType == that.cType
      case _ =>
        DsOliLogger.debug("DsOliCell::equals: called on wrong element ")
        false
    }
  }
}

object DsOliCell {
  type CellId = Long
  type CycleId = Long
  // Important to start from 1, as zero means 0 reference
  var id: Long = 1
  /**
   * Simple unique cell id generator
   * @return unique cell id
   */
  def getId(): Long = {
    val retId = id;
    id += 1
    return retId
  }
}
