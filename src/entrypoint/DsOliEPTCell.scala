
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
 * DsOliEPTCell.scala created on Jan 28, 2015
 *
 * Description: An entry pointer cell, which
 * is extended from a cell to handle the
 * additional information of the linkage
 * offset and the offset of the EPT cell
 * within the surrounding vertex.
 */
package entrypoint

import boxcalculation.DsOliCell
import pointstograph.DsOliType
import boxcalculation.DsOliBox.BoxId
import boxcalculation.DsOliCell._
import extlogger.DsOliLogger

/**
 * @author DSI
 *
 * @constructor creates an EPT cell
 * @param id a unique EPT cell id
 * @param bAddr the memory start address of this cell
 * @param eAddr the memory end address of this cell
 * @param cType the type associated with this cell
 * @param vId the id of the associated vertex
 * @param linkageOffset the linkage offset of the associated strand
 * @param cellOffset the offset of this cell in the surrounding vertex
 */
class DsOliEPTCell(override val id: CellId,
  override val bAddr: Long,
  override val eAddr: Long,
  override val cType: DsOliType,
  var vId: Long,
  val linkageOffset: Long,
  val cellOffset: Long) extends DsOliCell(id, bAddr, eAddr, cType, vId) {

  def this(cell: DsOliCell, linkageOffset: Long, cellOffset: Long) = this(cell.id, cell.bAddr, cell.eAddr, cell.cType, cell.vertexId, linkageOffset, cellOffset)

  /**
   * Deep copy of an EPT cell. Most importantly
   * the cell ID is carried over.
   *
   * @return a copy of the EPT cell
   */
  override def deepCopy(): DsOliEPTCell = {
    // Important to carry over the id
    val copy = new DsOliEPTCell(this.id, this.bAddr, this.eAddr, this.cType, this.vId,
      this.linkageOffset, this.cellOffset)
    return copy
  }

  override def toString(): String = {
    "[" + this.getClass() + ": id = " + id + "," + "bAddr = " +
      bAddr.toHexString + "," + "eAddr = " + eAddr.toHexString + "," + "cType = " + cType + "," + "vertexRef = " + vId +
      "linkageOffset = " + linkageOffset + ", cellOffset = " + cellOffset + "]"
  }

  override def equals(other: Any): Boolean = {
    if (other == null) return false
    return other match {
      case that: DsOliEPTCell =>
        DsOliLogger.debug("DsOliEPTCell::equals: testing against DsOliEPTCell. called on element " + id + " with " + that.id)
        super.equals(that) && this.linkageOffset == that.linkageOffset &&
          this.cellOffset == that.cellOffset
      case that: DsOliCell =>
        DsOliLogger.debug("DsOliEPTCell::equals: testing against DsOliCell. called on element " + id + " with " + that.id)
        super.equals(that)
      case _ =>
        DsOliLogger.debug("DsOliEPTCell::equals: called on wrong element ")
        false
    }
  }

}

object DsOliEPTCell {
  type CellId = Long
  type CycleId = Long
  // Important to start from 1, as zero means 0 reference
  var id: Long = 1

  /**
   * Simple unique EPT cell ID generator
   * @return unique EPT cell ID
   */
  def getId(): Long = {
    val retId = id;
    id += 1
    return retId
  }
}