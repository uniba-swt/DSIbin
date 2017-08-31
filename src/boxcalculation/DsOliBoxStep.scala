
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
 * BoxStep.scala created on Oct 29, 2014
 *
 * Description: Stores the strands for one
 * particular time step. Including cycles
 * and cells.
 */
package boxcalculation

import scala.collection.mutable.HashMap
import boxcalculation.DsOliBox._
import boxcalculation.DsOliCycle._
import boxcalculation.DsOliCell._
import extlogger.DsOliLogger
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

/**
 * @author DSI
 *
 */
class DsOliBoxStep {
  
  // All strands for a time step with a fast access via the strand id
  val boxes: HashMap[BoxId, DsOliBox] = new HashMap[BoxId, DsOliBox]()
  // All cycle for a time step with a fast access via the cycle id
  val cycles: HashMap[CycleId, DsOliCycle] = new HashMap[CycleId, DsOliCycle]()
  // All cells for a time step with a fast access via the cell id
  val cells: HashMap[CellId, DsOliCell] = new HashMap[CellId, DsOliCell]()
  
  val artificialBoxes = new ListBuffer[DsOliBoxStep]()

  /**
   * Deep copy of a time step, copying all strands, cycles and cells
   * 
   * @return instance of the copied time step
   */
  def deepCopy(): DsOliBoxStep = {
    val copy = new DsOliBoxStep()
    // Copy all boxes
    this.boxes.foreach {
      box =>
        copy.boxes.put(box._1, box._2.deepCopy(this))
    }
    // Copy all cycles
    this.cycles.foreach {
      cycle =>
        copy.cycles.put(cycle._1, cycle._2.deepCopy(this))
    }
    // Copy all cells
    this.cells.foreach {
      cell =>
        copy.cells.put(cell._1, cell._2.deepCopy())
    }
    return copy
  }

  /**
   * Find a cell in a list of cells
   * 
   * @param needle the cell to search for
   * @param haystack the list of cells to search in
   * @return Option the found cell
   */
  def lookupCellinCells(needle: DsOliCell, haystack: ListBuffer[DsOliCell]): Option[DsOliCell] = {
    DsOliLogger.debug("DsOliBoxStep::lookupCellinCells: entered. needle: " + needle + " haystack: " + haystack)
    var retCell: Option[DsOliCell] = None
    breakable {
      haystack.foreach {
        itCell =>
          DsOliLogger.debug("DsOliBoxStep::lookupCellinCells: checking cell: " + itCell)
          if (itCell == needle) {
            DsOliLogger.debug("DsOliBoxStep::lookupCellinCells: found cell")
            retCell = Some(itCell)
            break
          }
      }
    }
    return retCell
  }

  /**
   * Try to find the cell in the already present cells.
   * If not found, create the cell
   * 
   * @param cell the cell to search for
   * @return cell instance, either reused or newly created
   */
  def lookupOrUseCell(cell: DsOliCell): DsOliCell = {
    DsOliLogger.debug("DsOliBoxStep::lookupOrUseCell: entered. Need to find cell: " + cell)
    var retCell = cell
    breakable {
      // Check through each box
      this.boxes.foreach {
        boxTuple =>
          val (id, box) = boxTuple
          DsOliLogger.debug("DsOliBoxStep::lookupOrUseCell: checking cells of box: " + box)
          // Check linear part first
          val cellOpt = lookupCellinCells(cell, box.cells)
          if (cellOpt.isDefined) {
            retCell = cellOpt.get
            break
          } else {
            // Check cyclic part if present
            if (box.cycleId != 0) {
              this.cycles.foreach {
                cycleTuple =>
                  val (cycleId, cycle) = cycleTuple
                  DsOliLogger.debug("DsOliBoxStep::lookupOrUseCell: checking cells of cycle: " + cycle)
                  val cellCycleOpt = lookupCellinCells(cell, cycle.cells)
                  if(cellCycleOpt.isDefined){
                    retCell = cellCycleOpt.get
                    break
                  }
              }
            }
          }
      }
    }
    return retCell
  }

  /**
   * Helper for toString
   * 
   * @param padding formatting
   */
  def printBoxes(padding: String = ""): String = {
    var boxes = padding + "Boxes:\n"
    this.boxes.foreach {
      box =>
        boxes += padding + "\tBox: " + box._2 + "\n"
        boxes += padding + "\t\tid:" + box._1 + "\n"
    }
    return boxes
  }

  /**
   * Helper for toString
   * 
   * @param padding formatting
   */
  def printCycles(padding: String = ""): String = {
    var cycles = padding + "Cycles:\n"
    this.cycles.foreach {
      cycle =>
        cycles += padding + "\tCycle: " + cycle._2 + "\n"
        cycles += padding + "\t\tid:" + cycle._1 + "\n"
    }
    return cycles
  }

  /**
   * Helper for toString
   * 
   * @param padding formatting
   */
  def printCells(padding: String = ""): String = {
    var cells = padding + "Cells:\n"
    this.cells.foreach {
      cell =>
        cells += padding + "\tCycle: " + cell._2 + "\n"
        cells += padding + "\t\tid:" + cell._1 + "\n"
    }
    return cells
  }

  override def toString(): String = {
    "Current step object " + this.hashCode() + "\n" +
      printBoxes("\t") + "***\n" +
      printCycles("\t") + "***\n" +
      printCells("\t")

  }
}