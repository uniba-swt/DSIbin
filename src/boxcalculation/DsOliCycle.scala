
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
 * DsOliCycle.scala created on Oct 29, 2014
 *
 * Description: Represents the cyclic part of a strand
 */
package boxcalculation

import scala.collection.mutable.ListBuffer
import boxcalculation.DsOliCycle._
import boxcalculation.DsOliCell._

/**
 * @author DSI
 *
 * @constructor create the cyclic part of a strand
 * @param id unique cycle id
 * @param offset the linkage offset analogous to the strand
 * @param cells the list of cells representing the cycle
 */

class DsOliCycle(val id: CycleId, val offset: Long, val cells: ListBuffer[DsOliCell]) {

  // Convenience constructors
  def this(id: CycleId, offset: Long) = this(id, offset, new ListBuffer[DsOliCell])
  def this(offset: Long) = this(DsOliCycle.getId(), offset, new ListBuffer[DsOliCell])

  /**
   * Add a cell to the current list of cells
   *
   * @param cell the cell to add
   */
  def addCell(cell: DsOliCell): Unit = {
    this.cells += cell
  }

  /**
   * Add all cells to the current list of cells
   *
   * @param cells the cells to add
   */
  def addCells(cells: ListBuffer[DsOliCell]): Unit = {
    this.cells ++= cells
  }

  /**
   * Create a deep copy of this cycle (but reuse cells if
   * they were already present in the given time step)
   *
   *  @param boxStep the time step for the strand
   *  @return instance of the copied cycle
   */
  def deepCopy(boxStep: DsOliBoxStep): DsOliCycle = {
    // Important: Reuse the id!
    val copy = new DsOliCycle(this.id, this.offset)
    this.cells.foreach {
      cell =>
        // Try to reuse the cell if it was already created
        copy.cells += boxStep.lookupOrUseCell(cell.deepCopy())
    }
    return copy
  }

  /**
   * Fetch a cell by its id
   *
   * @param cellID the id of the cell
   * @return Option of the found cell
   */
  def getCellById(cellID: CellId): Option[DsOliCell] = {
    this.cells.foreach {
      cell =>
        if (cell.id == cellID)
          return Some(cell)
    }
    None
  }

  override def toString(): String = {
    "[" + this.getClass() + ": id = " + id + "," + "offset = " + offset.toHexString + "," + "cells = " + cells + "]"
  }

}

object DsOliCycle {
  type CycleId = Long
  // Important to start from 1, as zero means 0 reference
  var id: Long = 1
  /**
   * Simple unique cycle id generator
   * @return unique cycle id
   */
  def getId(): Long = {
    val retId = id;
    id += 1
    return retId
  }
}