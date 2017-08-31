
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
 * DsOliBox.scala created on Oct 29, 2014
 *
 * Description: The actual strand representation
 */
package boxcalculation

import scala.collection.mutable.ListBuffer
import boxcalculation.DsOliCycle._
import boxcalculation.DsOliCell._
import pointstograph.DsOliType

/**
 * @author DSI
 *
 * @constructor creates a strand instance
 * @param id unique strand id
 * @param offset the linkage offset for the strand
 * @param cType the type of the cell this strand is made of
 * @param cells list of the cells forming this strand
 * @param cycleId reference to a cycle, if strand is cyclic or a lasso
 * @param cycleEntryPoint indicate where the strand enters the cycle (if strand is cyclic)
 */
case class DsOliBox(var id: Long, var offset: Long, var cType: DsOliType, var cells: ListBuffer[DsOliCell], var cycleId: CycleId, var cycleEntryPoint: CellId) {

  // Convenience constructors
  def this(id: Long, offset: Long, cType: DsOliType, cycleId: Long, cycleEntryPoint: CycleId) = this(id, offset, cType, new ListBuffer[DsOliCell], cycleId, cycleEntryPoint)
  def this(offset: Long, cType: DsOliType, cells: ListBuffer[DsOliCell], cycleId: Long, cycleEntryPoint: CycleId) = this(DsOliBox.getId(), offset, cType, cells, cycleId, cycleEntryPoint)
  def this(offset: Long, cType: DsOliType, cycleId: Long, cycleEntryPoint: CycleId) = this(DsOliBox.getId(), offset, cType, new ListBuffer[DsOliCell], cycleId, cycleEntryPoint)
  def this(offset: Long, cType: DsOliType) = this(DsOliBox.getId(), offset, cType, new ListBuffer[DsOliCell], 0, 0)
  def this(offset: Long, cells: ListBuffer[DsOliCell]) = this(DsOliBox.getId(), offset, cells.head.cType, cells, 0, 0)

  /**
   * Create a deep copy of this strand (but reuse cells if
   * they were already present in the given time step)
   *
   *  @param boxStep the time step for the strand
   *  @return instance of the copied strand
   */
  def deepCopy(boxStep: DsOliBoxStep): DsOliBox = {
    val copy = new DsOliBox(this.id, this.offset, this.cType, this.cycleId, this.cycleEntryPoint)
    this.cells.foreach {
      cell =>
        // Try to reuse the cell if it was already created
        copy.cells += boxStep.lookupOrUseCell(cell.deepCopy())
    }
    return copy
  }

  override def toString(): String = {

    var cellString = ""
    this.cells.foreach {
      cellString += _ + ","
    }
    "[" + this.getClass() + ": id = " + id + "," + "offset = " + offset.toHexString + "," + "cType = " + cType + "," + "cells = " + cellString +
      "cycleId = " + cycleId + "," + "cycleEntryPoint = " + cycleEntryPoint + "]"
  }

  /**
   * Checks, if the current strand is cyclic or a lasso
   *
   * @return Boolean
   */
  def isCyclic(): Boolean = {
    cycleId != 0
  }
}

object DsOliBox {
  type BoxId = Long

  var id: Long = 0
  /**
   * Simple unique strand id generator
   * @return unique strand id
   */
  def getId(): Long = {
    val retId = id;
    id += 1
    return retId
  }
}