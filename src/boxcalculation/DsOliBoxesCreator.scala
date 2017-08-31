
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
 * DsOliBoxesCreator.scala created on Oct 29, 2014
 *
 * Description: Calculate strands (prior terminology
 * was boxes)
 */
package boxcalculation

import pointstograph.DsOliPointsToGraphs
import event.DsOliEvents
import event.DsOliMemoryEvent
import pointstograph.DsOliGraph
import pointstograph.DsOliVertexMemory
import pointstograph.DsOliVertex
import pointstograph.ITypeDB
import event.DsOliMWEvent
import extlogger.DsOliLogger
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer
import boxcalculation.DsOliCycle._
import test.DsOliTestMethods
import PartialFunction._
import event.DsOliArtificialUndefEvent
import event.DsOliArtificialFreeEvent
import event.DsOliMWEvent
import event.DsOliFreeEvent
import event.DsOliVLSEvent
import event.DsOliEvent
import util.DsOliAddressUtils
import event.DsOliLValue
import event.DsOliRValue

/**
 * @author DSI
 *
 * @constructor creates an instance of the strands creator
 * @param events the event trace
 * @param ptgs the points-to graphs for the event trace
 * @param typeDB the type store for DSI
 */
class DsOliBoxesCreator(val events: DsOliEvents, val ptgs: DsOliPointsToGraphs, val typeDB: ITypeDB) extends IDsOliBoxCreator {

  val classSignature = "DsOliBoxesCreator::"

  /**
   * Convert a given vertex into a memory vertex
   *
   * @param vertex the vertex to convert
   * @return the vertex casted to a memory vertex
   */
  def getMemoryVertex(vertex: DsOliVertex): DsOliVertexMemory = {
    val funSignature = classSignature + "getMemoryVertex: "
    DsOliLogger.debug(funSignature + "entered: ")
    vertex match {
      case v: DsOliVertexMemory => v
      case _ => throw new Exception("Not a memory vertex")
    }
  }

  /**
   * Checks if the given memory regions are overlapping
   *
   * @param sourceBegin the start address of the first memory region
   * @param sourceEnd the end address of the first memory region
   * @param targetBegin the start address of the second memory region
   * @param targetEnd the end address of the second memory region
   * @return Boolean
   */
  def regionsOverlapping(sourceBegin: Long, sourceEnd: Long, targetBegin: Long, targetEnd: Long): Boolean = {
    val funSignature = classSignature + "regionsOverlapping: "
    DsOliLogger.debug(funSignature + "entered: ")
    if (sourceBegin <= targetBegin && targetBegin <= sourceEnd ||
      targetBegin <= sourceBegin && sourceBegin <= targetEnd) {
      true
    } else {
      false
    }
  }

  /**
   * Tests if two cells fulfill the linkage conditions to form a linked list,
   * i.e., a strand
   *
   * @param sourceAddr the source address of the pointer
   * @param targetAddre the target address where the pointer points to
   * @param ptg the points-to graph
   * @param currentBoxes the current set of strands
   * @return Option tuple of the source and target cell with the linkage offset
   */
  def minCond(sourceAddr: Long, targetAddr: Long, ptg: DsOliGraph, currentBoxes: DsOliBoxStep): Option[(DsOliCell, DsOliCell, Long)] = {
    val funSignature = classSignature + "minCond: "
    DsOliLogger.debug(funSignature + "entered: ")
    val nullTuple: (DsOliCell, DsOliCell, Long) = (null, null, 0)

    // NULL?
    if (sourceAddr == 0 || targetAddr == 0) return None

    // Do source and target vertices exist for the addresses?
    val vertexSourceOpt = ptg.getVertexForAddress(sourceAddr)
    val vertexTargetOpt = if (vertexSourceOpt.isDefined) ptg.getVertexForAddress(targetAddr) else None
    if (vertexTargetOpt.isEmpty) return None

    // Get the vertices
    val vertexSource = getMemoryVertex(vertexSourceOpt.get)
    val vertexTarget = getMemoryVertex(vertexTargetOpt.get)

    // Calculate the offset into the target
    val targetOffset = targetAddr - vertexTarget.bAddr

    // Test if there exists a type at this boundary
    val targetCellTypeOpt = typeDB.isOffsetAtTypeBoundary(targetOffset, vertexTarget.vType.vType)
    if (targetCellTypeOpt.isEmpty) return None
    var targetCellTypes = targetCellTypeOpt.get

    // Calculate the source offset
    val sourceOffset = sourceAddr - vertexSource.bAddr

    // Test if the target type is also found in the source
    val sourceCellTypeOpt = typeDB.getMatchingTypeForOffset(sourceOffset, vertexSource.vType.vType, targetCellTypes)
    if (sourceCellTypeOpt.isEmpty) return None
    val sourceCellTypeOffset = sourceCellTypeOpt.get._1
    val sourceCellType = sourceCellTypeOpt.get._2
    val targetCellType = sourceCellType
    DsOliLogger.debug(funSignature + "sourceCellType: " + sourceCellType + ", targetCellType: " + targetCellType)

    val sourceCellBAddr = vertexSource.bAddr + sourceOffset - sourceCellTypeOffset
    val sourceCellEAddr = sourceCellBAddr + sourceCellType.size - 1

    DsOliLogger.debug(funSignature + "sourceCellBAddr: " + sourceCellBAddr.toHexString + ", sourceCellEAddr: " + sourceCellEAddr.toHexString)

    // Overlapping memory regions?
    val targetCellEAddr = targetAddr + targetCellType.size - 1
    if (regionsOverlapping(sourceCellBAddr, sourceCellEAddr,
      targetAddr, targetCellEAddr)) return None

    DsOliLogger.debug(funSignature + "calculated offset: " + sourceCellTypeOffset)

    // Try to lookup the cell and use it, if it already exists. Otherwise use the newly created cell
    Some(currentBoxes.lookupOrUseCell(new DsOliCell(sourceCellBAddr, sourceCellEAddr, sourceCellType, vertexSource.id)),
      currentBoxes.lookupOrUseCell(new DsOliCell(targetAddr, targetCellEAddr, targetCellType, vertexTarget.id)),
      sourceCellTypeOffset)

  }

  /**
   * Expand the given cycle into a sequence again, by breaking
   * it apart at the given cell.
   *
   * @param cell the cell where to split the cycle
   * @param cycle the cycle to split
   * @param currentBoxes the set of strands
   */
  def expandCycleIntoSequence(cell: DsOliCell, cycle: DsOliCycle, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "expandCycleIntoSequence: "
    DsOliLogger.debug(funSignature + "entered: ")
    DsOliLogger.debug(funSignature + "entered. cell: " + cell + ", cycle.id: " + cycle.id)

    // Find the corresponding strand for the cycle
    currentBoxes.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        box match {
          case DsOliBox(_, offset, cType, cells, cycleId, cycleEntry) =>

            // Strand with reference to the cycle
            if (cycle.id == cycleId) {

              DsOliLogger.debug(funSignature + "matched, doing expansion on box: " + box + " cycle.id: " + cycle.id)
              DsOliLogger.debug(funSignature + "cycle: " + cycle)

              // Get the entry cell for the cycle
              val cycleEntryCell = cycle.cells.find(_.id == cycleEntry).get

              // Convert the cell into an index of the list representation of the cycle
              val cycleEntryCellIndex = getCellIndexFromList(cycleEntryCell, cycle.cells).get
              val cycleEndIndex = getCellIndexFromList(cell, cycle.cells).get

              // Strand is an exclusive cycle, i.e., no linear cell sequence present -> copy all cells over
              if (box.cells.length == 0) {

                DsOliLogger.debug(funSignature + "exclusive cycle")
                DsOliLogger.debug(funSignature + "box.cells: " + box.cells)

                // Copy over everything after the cell
                if (cycleEndIndex < cycle.cells.length - 1) {
                  DsOliLogger.debug(funSignature + "cycleEndIndex < cycle.cells.length - 1")

                  val cellSlice = cycle.cells.slice(cycleEndIndex + 1, cycle.cells.length)
                  box.cells ++= cellSlice

                  DsOliLogger.debug(funSignature + " cellSlice: " + cellSlice)
                  DsOliLogger.debug(funSignature + "box.cells: " + box.cells)
                }

                DsOliLogger.debug(funSignature + "adding final slice")

                // Copy over everything before the cell
                val cellSlice = cycle.cells.slice(0, cycleEndIndex + 1)
                box.cells ++= cellSlice

                DsOliLogger.debug(funSignature + "cellSlice: " + cellSlice)
                DsOliLogger.debug(funSignature + "box.cells: " + box.cells)
              } else {
                // Strand is a lasso, i.e., has a linear part connected to a cycle

                // If the entry is at the start just copy over the complete cycle
                if (cycleEntryCell == 0 && cycleEndIndex == 0) {
                  box.cells ++= cycle.cells
                } // Detect if entry cell comes before source cell: slice directly
                else if (cycleEntryCellIndex <= cycleEndIndex) {
                  DsOliLogger.debug(funSignature + "direct slice")
                  DsOliLogger.debug(funSignature + "cycleEntryCellIndex: " + cycleEntryCellIndex + " cycleEndIndex: " + cycleEndIndex)
                  box.cells ++= cycle.cells.slice(cycleEntryCellIndex, cycleEndIndex + 1)
                } else {
                  // source cell comes before the entry cell: 
                  // (slice from entry cell to end) + (slice from beginning to entry cell - 1)
                  DsOliLogger.debug(funSignature + "two slices")
                  box.cells ++= cycle.cells.slice(cycleEntryCellIndex, cycle.cells.length)
                  box.cells ++= cycle.cells.slice(0, cycleEndIndex + 1)
                }

                DsOliLogger.debug(funSignature + "reconstructed box: " + box)
              }

              // Remove reference to cycle
              box.cycleId = 0
              box.cycleEntryPoint = 0

            } else {
              DsOliLogger.debug(funSignature + "cycle id mismatch.")
            }
          case _ =>
            DsOliLogger.debug(funSignature + "no match: " + box)
        }
    }
  }

  /**
   * Remove the cycle from each strand where target cell is the last
   * element.
   *
   * @param cycle the cycle to cut
   * @param targetCell the cell on which to cut
   * @param currentBoxes the strands to check
   */
  def cutCycle(cycle: DsOliCycle, targetCell: DsOliCell, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "cutCycle: "
    DsOliLogger.debug(funSignature + funSignature + "entered: ")
    currentBoxes.boxes.foreach {
      DsOliLogger.debug(funSignature + "entered. cycle: " + cycle + ", targetCell: " + targetCell)
      boxTuple =>
        val (boxId, box) = boxTuple
        DsOliLogger.debug(funSignature + "testing box: " + box)
        if (box.cycleId != 0 && cycle.offset == box.offset && box.cells.size > 0 && box.cells.last == targetCell) {
          DsOliLogger.debug(funSignature + "found box for cycle removal: " + box)
          box.cycleId = 0
          box.cycleEntryPoint = 0
        } else if (box.cycleId != 0 && cycle.offset == box.offset && box.cells.size == 0) {
          DsOliLogger.error(funSignature + "found box claims to have cycle, but has no cell elements: " + box)
        }
    }
  }

  /**
   * Remove the downstream strand part from all upstream strands
   * containing the downstream part.
   *
   * @param cell the cell where to cut
   * @param downstreamBox obsolete
   * @param offset the linkage offset
   * @param currentBoxes the strands to search
   */
  def cutDownstreamSeqFromUpstreamBoxes(cell: DsOliCell, downstreamBox: DsOliBox, offset: Long, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "cutDownstreamSeqFromUpstreamBoxes: "
    DsOliLogger.debug(funSignature + "entered: ")
    val delBoxes = new DsOliBoxStep()
    val newBoxes = new DsOliBoxStep()
    DsOliLogger.debug(funSignature + "entered. cell: " + cell + ", offset: " + offset)

    // Cycle through strands and cut off the downstream sequence
    currentBoxes.boxes.foreach {
      box =>
        DsOliLogger.debug(funSignature + "testing box id : " + box._1 + ": " + box._2)
        condOpt(box) {
          case (boxId, DsOliBox(_, boxOffset, _, cells, _, _)) =>

            // Linkage offsets need to match
            if (boxOffset == offset) {

              // Fetch the index of the cell in the strand
              val cellIndexOpt = getCellIndex(cell, box._2)
              if (cellIndexOpt.isDefined) {

                // Mark the old strand for removal
                DsOliLogger.debug(funSignature + "cellIndex found: " + cellIndexOpt.get)
                delBoxes.boxes.put(boxId, box._2)

                // New strand with downstream part cut off
                val newBox = new DsOliBox(offset, box._2.cType, getCellSliceUpToIndex(box._2, cellIndexOpt.get + 1), 0, 0)
                DsOliLogger.debug(funSignature + "new box created with id: " + newBox.id + ": " + newBox)

                newBoxes.boxes.put(newBox.id, newBox)
              } else {
                DsOliLogger.debug(funSignature + "index for cell not found: " + cell)
              }
            } else {
              DsOliLogger.debug(funSignature + "offset mismatch: offset:" + offset + ", boxOffset: " + boxOffset)
            }
        }
    }

    // Actually add the new strands
    newBoxes.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        currentBoxes.boxes.put(boxId, box)
        DsOliLogger.debug(funSignature + "adding box id: " + boxId)
    }

    // Cleanup old strands
    delBoxes.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        currentBoxes.boxes -= boxId
        DsOliLogger.debug(funSignature + "deleting box id: " + boxId)
    }

  }

  /**
   * If downstream is not covered by any existing strand, add it
   *
   * @param downstreamBox the downstream strand
   * @param offset the linkage offset
   * @param currentBoxes the set of strands to check
   */
  def conditionallyAddDownstreamBoxToBoxSet(downstreamBox: DsOliBox, offset: Long, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "conditionallyAddDownstreamBoxToBoxSet: "
    DsOliLogger.debug(funSignature + funSignature + "entered: ")
    DsOliLogger.debug(funSignature + "entered.")
    val downstreamCells = downstreamBox.cells
    if (!currentBoxes.boxes.exists {
      boxTuple =>
        val (boxId, box) = boxTuple
        box.offset == downstreamBox.offset && box.cells.containsSlice(downstreamCells)
    }) {
      DsOliLogger.debug(funSignature + "Add the downstream box: " + downstreamBox)
      currentBoxes.boxes.put(downstreamBox.id, downstreamBox)
    }
  }

  /**
   * Destruct strands due to an memory write event.
   * Results in removal or splitting of strands.
   *
   * @param event the memory write event
   * @param ptg the points-to graph from the previous time step
   * @param currentBoxes copy of the previous time step to represent the current changes
   */
  def destructBoxes(event: DsOliMWEvent, ptg: DsOliGraph, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "destructBoxes: "

    DsOliLogger.debug(funSignature + "entered")

    // Fetch the source address from the event
    val sourceAddress = event.lValueList.last.address

    // Fetch the target address, where the source last pointed to
    val targetAddressOpt = ptg.getCurrentTargetAddr(sourceAddress)
    val targetAddress: Long = if (targetAddressOpt.isDefined) targetAddressOpt.get else 0

    DsOliLogger.debug(funSignature + "searching for sAddr: " + sourceAddress.toHexString + ", tAddr: " + targetAddress.toHexString)

    // Test if the source and target established a linkage condition.
    // If yes, it is required to destruct the involved strands
    val sourceTargetCellsOpt = minCond(sourceAddress, targetAddress, ptg, currentBoxes)
    if (sourceTargetCellsOpt.isDefined) {

      val (sourceCell, targetCell, offset) = sourceTargetCellsOpt.get
      DsOliLogger.debug(funSignature + "minCond is fullfilled. " + sourceCell + ", " + targetCell + ", " + offset)

      // Is the current strand cyclic on the source cell?
      if (isCyclic(sourceCell, offset, currentBoxes)) {
        DsOliLogger.debug(funSignature + "isCyclic on sourceCell")

        val cycleOpt = getCycle(sourceCell, offset, currentBoxes)
        if (cycleOpt.isDefined) {
          DsOliLogger.debug(funSignature + "do expansion and remove cycle: " + cycleOpt.get)
          expandCycleIntoSequence(sourceCell, cycleOpt.get, currentBoxes)
          currentBoxes.cycles -= cycleOpt.get.id
        } else {
          throw new Exception(funSignature + "no cycle found for sourceCell: " + sourceCell)
        }

        // Is the current strand cyclic on the target cell?
      } else if (isCyclic(targetCell, offset, currentBoxes)) {
        DsOliLogger.debug(funSignature + "isCyclic on targetCell:" + targetCell)

        val cycleOpt = getCycle(targetCell, offset, currentBoxes)
        if (cycleOpt.isDefined) {
          DsOliLogger.debug(funSignature + "do expansion and remove cycle: " + cycleOpt.get)
          cutCycle(cycleOpt.get, sourceCell, currentBoxes)
        } else {
          throw new Exception(funSignature + "no cycle found for targetCell: " + targetCell)
        }
      } else {
        // Non cyclic strand
        DsOliLogger.debug(funSignature + "no cycle")

        // Fetch the downstream part of the strand starting at the target cell
        val downstreamBoxOpt = getDownstreamBox(targetCell, offset, currentBoxes)
        if (downstreamBoxOpt.isDefined) {
          val downstreamBox = downstreamBoxOpt.get

          // Cut the downstream strand from the upstream strands
          cutDownstreamSeqFromUpstreamBoxes(sourceCell, downstreamBox, offset, currentBoxes)

          // Add the downstream strand if no other strand covers this part
          conditionallyAddDownstreamBoxToBoxSet(downstreamBox, offset, currentBoxes)
        } else {
          DsOliLogger.error(funSignature + "no downstream box available.")
        }
      }
    } else {
      DsOliLogger.debug(funSignature + "minCond is not fullfilled.")
    }
  }

  /**
   * Checks, if the fulfilled linkage condition will lead to a cyclic strand
   *
   * @param sourceCell the source cell
   * @param targetCell the target Cell
   * @param offset the linkage offset
   * @param boxes the strands
   * @return Boolean
   */
  def willBeCyclic(sourceCell: DsOliCell, targetCell: DsOliCell, offset: Long, boxes: DsOliBoxStep): Boolean = {
    val funSignature = classSignature + "willBeCyclic: "
    DsOliLogger.debug(funSignature + funSignature + "entered: ")
    DsOliLogger.debug(funSignature + "entered.")
    return boxes.boxes.exists {
      _ match {
        case (boxId, box) =>

          // Debug
          DsOliLogger.debug(funSignature + "testing box: " + box)
          if (box.cycleId == 0) {
            DsOliLogger.debug(funSignature + "non cyclic box")
            DsOliLogger.debug(funSignature + "box.offset(" + box.offset + ") == offset(" + offset + ")")
            DsOliLogger.debug(funSignature + "box.cells.last.id(" + box.cells.last.id + ") == sourceCell.id(" + sourceCell.id + ")")
            DsOliLogger.debug(funSignature + "box.cells.exists(cell => cell.id == targetCell.id): " + box.cells.exists(cell => cell.id == targetCell.id))
          } else {
            DsOliLogger.debug(funSignature + "cyclic box! box.cycleId(" + box.cycleId + ") == 0: " + (box.cycleId == 0))
          }

          // Currently not a cyclic strand
          if (box.cycleId == 0 &&
            // Linkage offset matches
            box.offset == offset &&
            // The last cell is the source cell
            box.cells.last.id == sourceCell.id &&
            // The target cell exists inside of the strand
            box.cells.exists(cell => cell.id == targetCell.id)) {
            // Debug
            DsOliLogger.debug(funSignature + "will be cyclic on box: " + box)
            DsOliLogger.debug(funSignature + "will be cyclic on box sourceCell: " + sourceCell)
            DsOliLogger.debug(funSignature + "will be cyclic on box targetCell: " + targetCell)

            true
          } else {
            DsOliLogger.debug(funSignature + "no cycle detected in this iteration")
            false
          }
        case _ =>
          DsOliLogger.warning("no box tuple")
          false
      }

    }
  }

  /**
   * Checks, if the given cell is in a cycle
   *
   * @param targetCell the cell to check
   * @param offset the linkage offset
   * @param boxes the set of boxes for the current time step
   * @return Boolean
   */
  def isCyclic(targetCell: DsOliCell, offset: Long, boxes: DsOliBoxStep): Boolean = {
    val funSignature = classSignature + "isCyclic: "
    DsOliLogger.debug(funSignature + "entered. with target cell: " + targetCell)
    return getCycle(targetCell, offset, boxes).isDefined
  }

  /**
   * Fetch a cycle where the given cell participates
   *
   * @param cell the cell to check for
   * @param offset the linkage offset
   * @param boxes the set of boxes for the current time step
   * @return Option the cycle
   */
  def getCycle(cell: DsOliCell, offset: Long, boxes: DsOliBoxStep): Option[DsOliCycle] = {
    val funSignature = classSignature + "getCycle: "
    DsOliLogger.debug(funSignature + "entered: ")
    var retCycle: Option[DsOliCycle] = None
    breakable {
      boxes.cycles.foreach {
        _ match {
          case (cycleID, cycle) =>
            DsOliLogger.debug(funSignature + "testing on cycleID: " + cycleID + " and cycle: " + cycle)
            if (cycle.offset == offset && cycle.cells.exists { iterCell =>
              DsOliLogger.debug(funSignature + "exists: iterCell.id == cell.id: " + iterCell.id +
                "==" + cell.id + " iterCell: " + iterCell)
              iterCell.id == cell.id
            }) {
              DsOliLogger.debug(funSignature + "target cell is part of cycle")
              retCycle = Some(cycle)
              break
            } else {
              DsOliLogger.debug(funSignature + "no cycle detected in this iteration")
            }
          case _ =>
            DsOliLogger.debug(funSignature + "no box tuple")
        }
      }
    }
    return retCycle
  }

  /**
   * Fetch the index of a cell
   *
   * @param cell the cell to search for
   * @param box the strand to search in
   * @return Option the index
   */
  def getCellIndex(cell: DsOliCell, box: DsOliBox): Option[Int] = {
    val funSignature = classSignature + "getCellIndex: "
    DsOliLogger.debug(funSignature + "entered. cell: " + cell)
    getCellIndexFromList(cell, box.cells)
  }

  /**
   * Convert the cell into an index of the list representation of the cycle
   *
   * @param cell the cell to search for
   * @param cells the list representation of the cycle
   * @return Option the index of the cell
   */
  def getCellIndexFromList(cell: DsOliCell, cells: ListBuffer[DsOliCell]): Option[Int] = {
    val funSignature = classSignature + "getCellIndexFromList: "
    DsOliLogger.debug(funSignature + "entered. cell: " + cell)
    val cellIterator = cells.iterator
    var retCellIndex = 0
    var continue = true
    while (continue && cellIterator.hasNext) {
      val testCell = cellIterator.next
      if (testCell.id == cell.id) {
        DsOliLogger.debug(funSignature + "id match: testCell.id == cell.id : " + testCell.id + " == " + cell.id)
        continue = false
      } else {
        DsOliLogger.debug(funSignature + "mismatch: testCell.id != cell.id : " + testCell.id + " != " + cell.id)
      }
      retCellIndex += 1
    }
    if (!continue) {
      retCellIndex -= 1
      Some(retCellIndex)
    } else {
      None
    }
  }

  /**
   * Find a strand with the given cell and offset
   *
   * @param segmentCell the cell to find
   * @param offset the linkage offset
   * @param boxes the strands to search in
   * @return Option tuple with the cell index and the strand
   */
  def boxExistsWithOffsetAndCell(segmentCell: DsOliCell, offset: Long, boxes: DsOliBoxStep): Option[(Int, DsOliBox)] = {
    val funSignature = classSignature + "boxExistsWithOffsetAndCell: "
    DsOliLogger.debug(funSignature + "entered. segmentCell: " + segmentCell + ", offset: " + offset)
    // Cycle through all boxes
    boxes.boxes.foreach {
      boxTuple =>
        val (boxID, box) = boxTuple
        condOpt(box) {
          case DsOliBox(_, boxOffset, _, _, _, _) =>
            // Linkage offset needs to match
            if (boxOffset == offset) {
              DsOliLogger.debug(funSignature + "offset match.")
              // Cell needs to be present
              val cellIndexOpt = getCellIndex(segmentCell, box)
              if (cellIndexOpt.isDefined) {
                DsOliLogger.debug(funSignature + "cell index match: " + cellIndexOpt.get)
                return Some(cellIndexOpt.get, boxes.boxes.get(boxID).get)
              }
            } else {
              DsOliLogger.debug(funSignature + "offset mismatch match.")
            }
        }
    }
    return None
  }

  /**
   * Get the slice of cells from the start index to the end of the strand
   *
   * @param box the strand to slice
   * @param startIndex the index to start
   * @return list of cells
   */
  def getCellSliceToEnd(box: DsOliBox, startIndex: Int): ListBuffer[DsOliCell] = {
    val funSignature = classSignature + "getCellSliceToEnd: "
    DsOliLogger.debug(funSignature + "entered: ")
    return box.cells.slice(startIndex, box.cells.length)
  }

  /**
   * Get the slice of cells from the start of the strand until the end index
   *
   * @param box the strand to slice
   * @param endIndex the index to end
   * @param list of cells
   */
  def getCellSliceUpToIndex(box: DsOliBox, endIndex: Int): ListBuffer[DsOliCell] = {
    val funSignature = classSignature + "getCellSliceUpToIndex: "
    DsOliLogger.debug(funSignature + "entered: ")
    return box.cells.slice(0, endIndex)
  }

  /**
   * Get the slice of cells of the strand from the start index until the end index
   *
   * @param box the strand to slice
   * @param startIndex the index to start
   * @param endIndex the index to end
   * @param list of cells
   */
  def getCellSliceBetweenIndices(box: DsOliBox, startIndex: Int, endIndex: Int): ListBuffer[DsOliCell] = {
    val funSignature = classSignature + "getCellSliceBetweenIndices: "
    DsOliLogger.debug(funSignature + "entered: ")
    return if (endIndex + 1 > box.cells.length) {
      getCellSliceToEnd(box, startIndex)
    } else {
      box.cells.slice(startIndex, endIndex + 1)
    }
  }

  /**
   * Get the downstream strand starting at the given cell
   *
   * @param target the cell where to cut
   * @param offset the linkage offset
   * @param boxes the strand set
   */
  def getDownstreamBox(target: DsOliCell, offset: Long, boxes: DsOliBoxStep): Option[DsOliBox] = {
    val funSignature = classSignature + "getDownstreamBox: "
    DsOliLogger.debug(funSignature + "entered: ")

    val boxWithSegCellOpt = boxExistsWithOffsetAndCell(target, offset, boxes)
    if (boxWithSegCellOpt.isDefined) {

      val (cellIndex, downstreamBox) = boxWithSegCellOpt.get

      DsOliLogger.debug(funSignature + "box exists: cellIndex: " + cellIndex + ", downstreamBox: " + downstreamBox)

      val cellSlice = getCellSliceToEnd(downstreamBox, cellIndex)

      // Test, if we were slicing on the last element
      if (cellSlice.length > 0) {
        val newDownstreamBox = new DsOliBox(offset, downstreamBox.cType, cellSlice, downstreamBox.cycleId, downstreamBox.cycleEntryPoint)

        DsOliLogger.debug(funSignature + "new box created with id: " + newDownstreamBox.id)
        DsOliLogger.debug(funSignature + "box exists: cellIndex: " + cellIndex + ", cellSlice: " + cellSlice)
        DsOliLogger.debug(funSignature + "box exists: cellIndex: " + cellIndex + ", newDownstreamBox: " + newDownstreamBox)

        Some(newDownstreamBox)
      } else {
        DsOliLogger.debug(funSignature + "no downstream box exists (last element).")
        None
      }
    } else {
      DsOliLogger.debug(funSignature + "no downstream box exists")
      None
    }

  }

  /**
   * Create a upstream strand containing the cell
   *
   * @param cell the cell to create a strand for
   * @param offset the linkage offset of the strand to create
   * @return instance of set of strands containing the upstream strand only
   */
  def createBoxFromCellAndOffset(cell: DsOliCell, offset: Long): DsOliBoxStep = {
    val funSignature = classSignature + "createBoxFromCellAndOffset: "
    DsOliLogger.debug(funSignature + "entered: ")
    val tmpUpstream = new DsOliBoxStep()
    val upstreamCells = new ListBuffer[DsOliCell]()
    upstreamCells += cell
    val upstreamBox = new DsOliBox(offset, upstreamCells)
    DsOliLogger.debug(funSignature + "new box created with id: " + upstreamBox.id + ": " + upstreamBox)
    tmpUpstream.boxes.put(upstreamBox.id, upstreamBox)
    return tmpUpstream
  }

  /**
   * Either use the strand set Option or create a
   * new strand set.
   *
   * @param boxes Option with strand set
   * @param cell the cell to create strand from
   * @param offset the linkage offset for the strand
   * @return instance of strand sets
   */
  def createOrUseBox(boxes: Option[DsOliBoxStep], cell: DsOliCell, offset: Long): DsOliBoxStep = {
    val funSignature = classSignature + "createOrUseBox: "
    DsOliLogger.debug(funSignature + "entered: ")
    return if (boxes.isDefined) {
      boxes.get
    } else {
      createBoxFromCellAndOffset(cell, offset)
    }
  }

  /**
   * Merge two strands. Second strand is appended
   * to first strand
   *
   * @param first the first strand
   * @param second the second strand
   * @param currentBoxes set of strands
   */
  def merge(first: DsOliBox, second: DsOliBox, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "merge: "
    DsOliLogger.debug(funSignature + "entered: ")
    first match {
      case DsOliBox(_, offset, _, _, 0, 0) =>
        val firstDeepCopy = first.deepCopy(currentBoxes)
        val secondDeepCopy = second.deepCopy(currentBoxes)
        val tmpBox = new DsOliBox(offset, first.cType, firstDeepCopy.cells ++= secondDeepCopy.cells, second.cycleId, second.cycleEntryPoint)
        DsOliLogger.debug(funSignature + "new box created with id: " + tmpBox.id + ": " + tmpBox)
        currentBoxes.boxes.put(tmpBox.id, tmpBox)
      case _ => throw new Exception(funSignature + "first box has a cycle: " + first)
    }
  }

  /**
   * Each upstream strand gets merged with the downstream strand
   *
   * @param upstream the set of upstream strands
   * @param downstreamBox the downstream strand
   * @param currentBoxes the strand set containing all strands
   */
  def mergeBoxes(upstream: DsOliBoxStep, downstreamBox: DsOliBox, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "mergeBoxes: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Creation
    upstream.boxes.foreach {
      condOpt(_) {
        case (boxId, box) =>
          DsOliLogger.debug(funSignature + "merging: upstream box: " + box + ", downstream box: " + downstreamBox)
          merge(box, downstreamBox, currentBoxes)
      }
    }

  }

  /**
   * Remove all strands, which are left over, i.e,
   * the upstream strands as they got merged and
   * the downstream strand if present.
   *
   * @param upstream the set of upstream strands
   * @param downstreamBox the downstream strand
   * @param currentBoxes the set of strands
   */
  def removeObsoleteBoxes(upstream: DsOliBoxStep, downstreamBox: DsOliBox, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "removeObsoleteBoxes: "
    DsOliLogger.debug(funSignature + "entered: ")
    // Cleanup of upstream boxes
    upstream.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        currentBoxes.boxes -= boxId
        DsOliLogger.debug(funSignature + "deleting upstream box id: " + boxId)
    }

    // Downstream box might not be present (due to new creation).
    // Only remove if present
    if (currentBoxes.boxes.contains(downstreamBox.id))
      currentBoxes.boxes -= downstreamBox.id
  }

  /**
   * Create a cycle for the source cell and the target cell
   *
   * @param sourceCell the source cell
   * @param targetCell the target cell
   * @param offset the linkage offset
   * @param currentBoxes the strands to search in
   * @return Option the id of the create cycle
   */
  def createCycle(sourceCell: DsOliCell, targetCell: DsOliCell, offset: Long, currentBoxes: DsOliBoxStep): Option[CycleId] = {
    val funSignature = classSignature + "createCycle: "
    DsOliLogger.debug(funSignature + "entered. sourceCell: " + sourceCell + ", targetCell: " + targetCell + ", offset: " + offset)
    // Cycle through strands
    currentBoxes.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        box match {
          case DsOliBox(_, boxOffset, _, cells, 0, 0) =>

            // Find strand with correct offset
            if (boxOffset == offset) {
              DsOliLogger.debug(funSignature + "Found box " + box + " without a cycle. cells: " + cells)

              // Test if strand contains both source and target
              if (box.cells.contains(sourceCell) && box.cells.contains(targetCell)) {
                DsOliLogger.debug(funSignature + "box cells contain source and target cell")

                // Create new cycle
                val newCycle = new DsOliCycle(offset)

                // Fetch the indices of the source and the target
                val targetIndexOpt = getCellIndex(targetCell, box)
                val sourceIndexOpt = getCellIndex(sourceCell, box)

                // Only if both are found fill the cycle with the sliced cells
                if (targetIndexOpt.isDefined && sourceIndexOpt.isDefined) {
                  val targetIndex = targetIndexOpt.get
                  val sourceIndex = sourceIndexOpt.get
                  DsOliLogger.debug(funSignature + "creating slice: targetIndex: " + targetIndex + ", sourceIndex: " + sourceIndex)

                  // Fetch the cells that form the cycle
                  val cycleCells = getCellSliceBetweenIndices(box, targetIndex, sourceIndex)
                  newCycle.addCells(cycleCells)
                  currentBoxes.cycles.put(newCycle.id, newCycle)

                  DsOliLogger.debug(funSignature + "new cycle: " + newCycle)

                  return Some(newCycle.id)
                } else {
                  throw new Exception("source/target index not found: " + targetCell + ", " + sourceCell)
                }
              } else {
                DsOliLogger.debug(funSignature + "box cells do not contain source and target cell")
              }
            } else {
              DsOliLogger.debug(funSignature + "offset mismatch")
            }

          case _ =>
            DsOliLogger.debug(funSignature + "Found box with cycle: " + box)
        }
    }
    return None
  }

  /**
   * Remove all cells from the cycle from the strand
   *
   * @param cells the cells of the strand
   * @param cycleCells the cells of the cycle
   * @return Option tuple the start cell and the remaining cells
   */
  def removeCycleElements(cells: ListBuffer[DsOliCell], cycleCells: ListBuffer[DsOliCell]): Option[(DsOliCell, ListBuffer[DsOliCell])] = {
    val funSignature = classSignature + "removeCycleElements: "
    DsOliLogger.debug(funSignature + funSignature + "entered: ")
    DsOliLogger.debug(funSignature + "entered. cycleCells: " + cycleCells)

    var cellIter = cells.last
    val cellIterIndexOpt = getCellIndexFromList(cellIter, cells)
    var cellIterIndex = cellIterIndexOpt.get
    DsOliLogger.debug(funSignature + "cellIter: " + cellIter)
    var cellInCycle = cycleCells.contains(cellIter)
    DsOliLogger.debug(funSignature + "cellIterIndex: " + cellIterIndex + ", cellInCycle: " + cellInCycle)

    // Iterate backwards over the linear strand sequence
    // and check, if the cell is still present in the cycle
    while (cellIterIndex > 0 && cellInCycle) {
      cellIterIndex -= 1
      cellIter = cells(cellIterIndex)
      DsOliLogger.debug(funSignature + "testing: cellIter: " + cellIter)
      cellInCycle = cycleCells.contains(cellIter)
      DsOliLogger.debug(funSignature + "cellIterIndex: " + cellIterIndex + ", cellInCycle: " + cellInCycle)
    }

    if (!cellInCycle) {

      DsOliLogger.debug(funSignature + "constructing entry point with index: " + cellIterIndex)
      DsOliLogger.debug(funSignature + "cells: " + cells)
      DsOliLogger.debug(funSignature + "cells.slice: " + cells.slice(0, cellIterIndex + 1))

      // Need to revert the cellIterIndex by one to get the start cell
      // Slice from start of strand to last covered cycle element
      Some((cells(cellIterIndex + 1), cells.slice(0, cellIterIndex + 1)))
    } else {
      // Everything was removed from the strand -> exclusive cycle strand
      DsOliLogger.debug(funSignature + "everything was removed (complete cycle)")
      Some((cells(0), new ListBuffer()))
    }

  }

  /**
   * Add a cycle reference to each strand participating in the given cycle
   * and remove the elements of the cycle from the participating strand
   *
   * @param id the cycle id
   * @param currentBoxes the strands to search in
   *
   */
  def compressSequenceIntoCycle(id: CycleId, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "compressSequenceIntoCycle: "
    DsOliLogger.debug(funSignature + "entered: ")
    DsOliLogger.debug(funSignature + "entered. id " + id)

    // Try to find the cycle 
    val cycleOpt = currentBoxes.cycles.get(id)
    if (cycleOpt.isDefined) {
      val cycle = cycleOpt.get

      // Check through all strands
      currentBoxes.boxes.foreach {
        boxTuple =>
          val (boxId, box) = boxTuple
          box match {
            case DsOliBox(_, boxOffset, _, cells, 0, 0) =>

              // Linkage offset needs to match
              if (boxOffset == cycle.offset) {
                DsOliLogger.debug(funSignature + "found box with offset")

                // Check if the last cell of the linear sequence of the strand
                // participates in the cycle.
                if (cycle.cells.contains(cells.last)) {
                  DsOliLogger.debug(funSignature + "cells.last contained in cycle.cells")
                  val cyclePropertiesOpt = removeCycleElements(cells, cycle.cells)
                  if (cyclePropertiesOpt.isDefined) {
                    val (startCell, remainingCells) = cyclePropertiesOpt.get
                    val newBox = new DsOliBox(box.offset, box.cType, remainingCells, cycle.id, startCell.id)
                    currentBoxes.boxes -= box.id
                    currentBoxes.boxes.put(newBox.id, newBox)
                  }

                } else {
                  DsOliLogger.debug(funSignature + "cells.last is not an element of cycle.cells")
                }
              } else {
                DsOliLogger.debug(funSignature + "offset mismatch")
              }
            case _ =>
              DsOliLogger.debug(funSignature + "no match")
          }
      }
    } else {
      throw new Exception("id not defined: " + id)
    }
  }

  /**
   * Append the cycle to each strand where the last cell is
   * the source cell.
   *
   * @param cycle the cycle to append
   * @param sourceCell the cell to consider
   * @param targetCell the entry point to the cycle
   * @param currentBoxes the strands to search in
   */
  def appendCycle(cycle: DsOliCycle, sourceCell: DsOliCell, targetCell: DsOliCell, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "appendCycle: "
    DsOliLogger.debug(funSignature + "entered. cycle: " + cycle + ", sourceCell: " + sourceCell + ", targetCell: " + targetCell)

    // Cycle through all strands
    currentBoxes.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        condOpt(box) {
          case DsOliBox(_, offset, _, cells, 0, 0) =>
            // Append the cell to each strand where the linkage offset matches
            // and the last cell equals the source cell
            if (offset == cycle.offset && cells.last == sourceCell) {
              DsOliLogger.debug(funSignature + "found box to append cycle to: " + box)
              box.cycleEntryPoint = targetCell.id
              box.cycleId = cycle.id
            }
        }
    }
  }

  /**
   * Fetch the set of upstream strands
   *
   * @param cell the cell to test
   * @param offset the linkage offset
   * @param currentBoxes the strands to search
   * @return Option set of upstream strands
   */
  def getUpstreamBoxes(cell: DsOliCell, offset: Long, currentBoxes: DsOliBoxStep): Option[DsOliBoxStep] = {
    val funSignature = classSignature + "getUpstreamBoxes: "
    DsOliLogger.debug(funSignature + funSignature + "entered: ")
    DsOliLogger.debug(funSignature + "entered. cell: " + cell + ", offset: " + offset)
    val upstreamBoxes = new DsOliBoxStep()
    currentBoxes.boxes.foreach {
      box =>
        DsOliLogger.debug(funSignature + "testing box id : " + box._1 + ": " + box._2)
        condOpt(box) {
          case (boxId, DsOliBox(_, boxOffset, _, cells, _, _)) =>
            DsOliLogger.debug(funSignature + "testing for offset matches on boxes: " + offset + "==" + box._2.offset + " boxOffset: " + boxOffset)
            // Do some additional tests on the cells: not null and size greater zero 
            // to be able to use the method in the cyclic part of the algorithm
            if (boxOffset == offset && cells != null && cells.size > 0 && cells.last == cell) {
              DsOliLogger.debug(funSignature + "cells.last equals cell.")
              upstreamBoxes.boxes.put(boxId, box._2)
            } else {
              DsOliLogger.debug(funSignature + "offsets do NOT match OR cells.last NOT equal cell.")
            }
        }
    }
    return if (upstreamBoxes.boxes.size > 0) Some(upstreamBoxes) else None
  }

  /**
   * Fetch the downstream strand that starts with the
   * given cell, if there exists one
   *
   * @param targetCell the target cell to search for
   * @param offset the linkage offset
   * @param currentBoxes the strand set
   * @return Option the strand instance
   */
  def getDownstreamBoxIfExists(targetCell: DsOliCell, offset: Long, currentBoxes: DsOliBoxStep): Option[DsOliBox] = {
    val funSignature = classSignature + "getDownstreamBoxIfExists: "
    DsOliLogger.debug(funSignature + "entered.")
    val foundBoxOpt = currentBoxes.boxes.find {
      boxTuple =>
        val (boxId, box) = boxTuple
        box.offset == offset && box.cells.length > 0 && box.cells.head == targetCell
    }

    if (foundBoxOpt.isDefined) {
      DsOliLogger.debug(funSignature + "Found downstream box: " + foundBoxOpt.get._2)
      Some(foundBoxOpt.get._2)
    } else {
      None
    }

  }

  /**
   * Create the downstream strand either from
   * an existing strand containing the target
   * cell or the given target cell
   *
   * @param targetCell the target cell
   * @param offset linkage offset
   * @param currentBoxes strand set to search in
   * @return the strand instance
   */
  def createDownstreamBox(targetCell: DsOliCell, offset: Long, currentBoxes: DsOliBoxStep): DsOliBox = {
    val funSignature = classSignature + "createDownstreamBox: "
    DsOliLogger.debug(funSignature + "entered: ")
    currentBoxes.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        condOpt(box) {
          case DsOliBox(_, boxOffset, cType, cells, cycleId, cycleEntryPoint) =>
            if (boxOffset == offset) {
              DsOliLogger.debug(funSignature + "found box with offset")
              if (cells.contains(targetCell)) {
                DsOliLogger.debug(funSignature + "targetCell contained in cells")
                val targetCellIndexOpt = getCellIndex(targetCell, box)
                if (targetCellIndexOpt.isDefined) {
                  val downstreamBox = new DsOliBox(offset, cType, getCellSliceToEnd(box, targetCellIndexOpt.get), cycleId, cycleEntryPoint)
                  return downstreamBox
                } else {
                  DsOliLogger.error("")
                }
              } else {
                DsOliLogger.debug(funSignature + "targetCell not contained in cells")
              }
            } else {
              DsOliLogger.debug(funSignature + "offset mismatch")
            }

        }
    }
    val downstreamBoxStep = createOrUseBox(None, targetCell, offset)
    downstreamBoxStep.boxes.toList.head._2
  }

  /**
   * Construct strands in case of a memory write event
   *
   * @param event the memory write event
   * @param ptg the points-to graph
   * @param currentBoxes the set of strands
   */
  def constructBoxes(event: DsOliMWEvent, ptg: DsOliGraph, currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "constructBoxes: "
    DsOliLogger.debug(funSignature + funSignature + "entered: ")

    // Fetch the source and target address of the event
    val sourceAddress = event.lValueList.last.address
    val targetAddress = event.rValue.content

    DsOliLogger.debug(funSignature + "searching for sAddr: " + sourceAddress.toHexString + ", tAddr: " + targetAddress.toHexString)

    // Check if a linkage condition is fulfilled
    val sourceTargetCellsOpt = minCond(sourceAddress, targetAddress, ptg, currentBoxes)
    if (sourceTargetCellsOpt.isDefined) {

      val (sourceCell, targetCell, offset) = sourceTargetCellsOpt.get
      DsOliLogger.debug(funSignature + "minCond is fullfilled. " + sourceCell + ", " + targetCell + ", " + offset)

      // Check, if the strand will be cyclic
      if (willBeCyclic(sourceCell, targetCell, offset, currentBoxes)) {
        val cycleIdOpt = createCycle(sourceCell, targetCell, offset, currentBoxes)
        if (cycleIdOpt.isDefined) {
          compressSequenceIntoCycle(cycleIdOpt.get, currentBoxes)
        } else {
          throw new Exception("no cycle id found")
        }
      } else if (isCyclic(targetCell, offset, currentBoxes)) {
        // The target is cyclic

        val cycleOpt = getCycle(targetCell, offset, currentBoxes)
        if (cycleOpt.isDefined) {
          DsOliLogger.debug(funSignature + "targetCell is cyclic.")

          // Try to fetch an upstream strand for the cell
          val upstreamOpt = getUpstreamBoxes(sourceCell, offset, currentBoxes)

          // If none was found, create a strand for the cell
          if (upstreamOpt.isEmpty) {
            DsOliLogger.debug(funSignature + "no box present for source cell. Creating new box.")
            val newSourceBoxStep = createBoxFromCellAndOffset(sourceCell, offset)
            val (newSourceBoxId, newSourceBox) = newSourceBoxStep.boxes.head
            currentBoxes.boxes.put(newSourceBoxId, newSourceBox)
          }

          DsOliLogger.debug(funSignature + "appending cycle.")

          // Append the cycle to the strands
          appendCycle(cycleOpt.get, sourceCell, targetCell, currentBoxes)
        } else {
          throw new Exception("no cycle found for targetCell: " + targetCell)
        }
      } else {
        // No cyclicity

        val upstreamOpt = getUpstreamBoxes(sourceCell, offset, currentBoxes)

        // Create the upstream part, if it doesn't exist
        val upstream = createOrUseBox(upstreamOpt, sourceCell, offset)

        // Try to fetch the downstream box
        val downstreamOpt = getDownstreamBoxIfExists(targetCell, offset, currentBoxes)

        // Create the downstream part if it doesn't exist
        val downstream = if (downstreamOpt.isDefined) downstreamOpt.get else createDownstreamBox(targetCell, offset, currentBoxes)

        mergeBoxes(upstream, downstream, currentBoxes)

        removeObsoleteBoxes(upstream, downstream, currentBoxes)
      }

    } else {
      DsOliLogger.debug(funSignature + "minCond is not fullfilled.")
    }
  }

  /**
   * Check, if the given cycle is referenced
   * by any strand
   *
   * @param currentBoxes the set of strands to search in
   * @param offset the linkage offset
   * @param cycleId the ID of the cycle to search for
   * @return Boolean
   */
  def cycleIsReferenced(currentBoxes: DsOliBoxStep, offset: Long, cycleId: CycleId): Boolean = {
    val funSignature = classSignature + "cycleIsReferenced: "
    DsOliLogger.debug(funSignature + "entered: ")
    currentBoxes.boxes.exists {
      checkBoxTuple =>
        val (checkBoxId, checkBox) = checkBoxTuple
        checkBox match {
          case DsOliBox(_, offset, _, checkCells, boxCycleId, _) =>
            // Check for the cycle
            if (boxCycleId == cycleId) {
              DsOliLogger.debug(funSignature + "cycle ids match.")
              // Make sure, that the strand is not an exclusive cycle,
              // i.e, the list of cells contains elements
              if (checkCells != null && checkCells.length != 0) {
                DsOliLogger.debug(funSignature + "found box which references cycle of exclusive cycle box.")
                true
              } else {
                false
              }
            } else {
              DsOliLogger.debug(funSignature + "cycle ids mismatch.")
              false
            }
          case _ => false
        }
    }
  }

  /**
   * Cleanup the strands:
   * - remove the strands which exclusively represent a cycle,
   *   if the cycle is referenced in any other strand
   * - add new strand which exclusively represents a cycle
   *   if the cycle was not referenced in any strand yet
   *
   * @param currentBoxes the strands to clean up
   */
  def cleanupBoxes(currentBoxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "cleanupBoxes: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Cleanup the boxes which exclusively represent a cycle
    var delBoxes = Set[DsOliBox]()
    currentBoxes.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        box match {
          case DsOliBox(_, offset, _, cells, cycleId, _) =>
            if (cells == null || cells.length == 0) {
              DsOliLogger.debug(funSignature + "found exclusive cycle box.")
              if (cycleIsReferenced(currentBoxes, offset, cycleId)) {
                delBoxes += box
              }
            }
          case _ =>
            DsOliLogger.debug(funSignature + "unmatched box: " + box.id)
        }
    }
    delBoxes.foreach {
      box =>
        DsOliLogger.debug(funSignature + "removing box: " + box)
        currentBoxes.boxes -= box.id
    }

    // Create exclusive cycle boxes for unreferenced cycles
    var addBoxes = Set[DsOliBox]()
    currentBoxes.cycles.foreach {
      cycleTuple =>
        val (cycleId, cycle) = cycleTuple
        DsOliLogger.debug(funSignature + "testing cycle: " + cycle)
        if (!currentBoxes.boxes.exists {
          boxTuple =>
            val (_, box) = boxTuple
            if (box.cycleId == cycleId) true else false
        }) {
          DsOliLogger.debug(funSignature + "found no box with cycle id: " + cycleId)
          val tmpBox = new DsOliBox(cycle.offset, cycle.cells(0).cType, cycle.id, cycle.cells(0).id)
          DsOliLogger.debug(funSignature + "new box created with id: " + tmpBox.id + ": " + tmpBox)
          addBoxes += tmpBox
        }
    }

    addBoxes.foreach {
      box =>
        DsOliLogger.debug(funSignature + "adding box: " + box)
        currentBoxes.boxes.put(box.id, box)
    }
  }

  /**
   * Vertex IDs might change, so update the IDs inside the cells
   *
   * @param currentBoxes the strands to search through
   * @param ptg the points-to graph to search through
   */
  def recalculateCellVertexReferences(currentBoxes: DsOliBoxStep, ptg: DsOliGraph): Unit = {
    val funSignature = classSignature + "recalculateCellVertexReferences: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Cycle through strands
    currentBoxes.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        // Cycle through cells
        box.cells.foreach {
          cell =>
            // Cycle through vertex nodes
            ptg.graph.nodes.iterator.foreach {
              nodeOuter =>
                condOpt(nodeOuter.value) {
                  case v: DsOliVertexMemory =>
                    if (v.bAddr <= cell.bAddr && cell.eAddr <= v.eAddr) {
                      DsOliLogger.debug(funSignature + "found vertex.id: " + v.id + " for cell with vertex.id: " + cell.vertexId)
                      cell.vertexId = v.id;
                    }
                }

            }
        }
    }
  }

  /**
   * Remove all strands affected by the free / VLS
   *
   * @param event the free / VLS event
   * @param ptg the points-to graph
   * @param boxes the strands to search in
   */
  def freeVLSForBoxes(event: DsOliEvent, ptg: DsOliGraph, boxes: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "freeVLSForBoxes: "

    DsOliLogger.debug(funSignature + "entered: event: " + event)
    DsOliLogger.debug(funSignature + "\tptg: " + ptg)
    DsOliLogger.debug(funSignature + "\tboxes: " + boxes)

    // Get the address from the event
    val addressOpt = DsOliAddressUtils.getStartAddressFromFreeVLS(event)
    if (addressOpt.isDefined) {
      // Fetch the corresponding vertex
      val vOpt = ptg.getVertexForAddress(addressOpt.get)
      if (vOpt.isDefined) {
        val v: DsOliVertexMemory = ptg.getMemoryVertex(vOpt.get)
        val delBoxes = new ListBuffer[DsOliBox]()

        // Cycle through all strands
        boxes.boxes.foreach {
          boxTuple =>
            val (boxId, box) = boxTuple

            // Search for a cell in the box, which lies within the freed vertex
            if (box.cells.exists(cell => DsOliAddressUtils.addressInRange(v.bAddr, v.eAddr, cell.bAddr, cell.eAddr))) {
              // At this point all edges should be cut, so just remove the box completely
              delBoxes += box
            }

        }

        // Remove all boxes
        delBoxes.foreach {
          boxes.boxes -= _.id
        }
      } else {
        DsOliLogger.warning(funSignature + "no vertex found for address: " + addressOpt.get.toHexString)
        DsOliLogger.warning(funSignature + "\tevent: " + event)
      }

    }

  }

  /**
   * Iterate through each event of the event trace and
   * calculate the strands for each time step.
   */
  override def createBoxes(): DsOliBoxSteps = {
    val funSignature = classSignature + "createBoxes: ";

    // Store for the calculated strands
    val boxesForSteps = new DsOliBoxSteps()
    // Empty first step
    val stepZero = new DsOliBoxStep()

    DsOliLogger.debug(funSignature + "entered")
    boxesForSteps.append(stepZero)

    // Cycle through all events
    var i = 0
    this.events.events.foreach {
      event =>
        i += 1
        print("Boxes: " + i + "/" + this.events.events.size + "\r")
        DsOliLogger.debug(funSignature + "#Event " + event.id + "# Step " + i + " box event " + event.id + "  ****")

        // Fetch the strands from the previous time step
        val currentBoxesOpt = boxesForSteps.get(i - 1)
        val currentBoxes = if (currentBoxesOpt.isDefined) {
          currentBoxesOpt.get.deepCopy
        } else {
          throw new Exception(funSignature + "Unable to get boxes from previous step: " + i)
        }

        // Fetch the previous PTG
        var prevPtg = ptgs.get(i - 1)

        // Do we have artificial events? The artificial events also include the original
        // event which triggered the creation of the artificial events
        if (event.artificialEvents.length != 0) {
          DsOliLogger.debug(funSignature + "event has artificial events: " + event)
          // Get the current ptg, which in case of artificial events
          // is the first graph in the list
          var u = 0
          var curPtg: DsOliGraph = null
          // Process all intermediate artificial events
          event.artificialEvents.foreach {
            artificialEvent =>
              DsOliLogger.debug("\t" + funSignature + "artificial event: " + artificialEvent)
              curPtg = ptgs.get(i).artificialGraphs(u)
              u += 1
              artificialEvent match {
                case e: DsOliArtificialUndefEvent =>
                  DsOliLogger.debug("\t" + funSignature + "artificial undef event: " + e)
                  destructBoxes(e, prevPtg, currentBoxes)
                  cleanupBoxes(currentBoxes)
                case e: DsOliMWEvent =>
                  DsOliLogger.debug("\t" + funSignature + "original artificial memory write event: " + e)
                  destructBoxes(e, prevPtg, currentBoxes)
                  constructBoxes(e, curPtg, currentBoxes)
                  cleanupBoxes(currentBoxes)
                case e: DsOliArtificialFreeEvent =>
                  DsOliLogger.debug("\t" + funSignature + "artificial free event: " + e)
                  freeVLSForBoxes(e, prevPtg, currentBoxes)
                case e: DsOliFreeEvent =>
                  DsOliLogger.debug("\t" + funSignature + "original artificial free event: " + e)
                  freeVLSForBoxes(e, prevPtg, currentBoxes)
                case e: DsOliVLSEvent =>
                  DsOliLogger.debug("\t" + funSignature + "original artificial vls event: " + e)
                  freeVLSForBoxes(e, prevPtg, currentBoxes)
                case _ => throw new Exception(funSignature + "unknown event: " + artificialEvent)
              }
              prevPtg = curPtg

              // Record the artificial boxes
              currentBoxes.artificialBoxes += currentBoxes.deepCopy
          }
        } else {

          // No artificial events

          // Fetch the current PTGs
          var curPtg = ptgs.get(i)

          event match {
            case e: DsOliMWEvent =>
              DsOliLogger.debug(funSignature + "memory write event")
              DsOliLogger.debug(funSignature + "recalculate cell references to vertex, due to new vertex creation")
              recalculateCellVertexReferences(currentBoxes, ptgs.get(i))

              destructBoxes(e, ptgs.get(i - 1), currentBoxes)
              constructBoxes(e, ptgs.get(i), currentBoxes)
              cleanupBoxes(currentBoxes)
              DsOliLogger.debug(funSignature + "finished memory write event")
            case e: DsOliVLSEvent =>
            // Handled by artificial events
            case _ => // Nothing

          }
        }

        DsOliLogger.debug(funSignature + "createdBoxes: " + currentBoxes)
        boxesForSteps.append(currentBoxes)

        DsOliLogger.debug(funSignature + "#Done Event " + event.id + "# Ending with step " + i + "****\n")

    }
    return boxesForSteps
  }
}