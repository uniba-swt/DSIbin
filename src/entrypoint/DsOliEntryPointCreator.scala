
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
 * DsOliEntryPointCreator.scala created on Jan 28, 2015
 *
 * Description: Calculate EPTs for observing changes
 * of the data structures
 */
package entrypoint

import boxcalculation.DsOliBoxSteps
import pointstograph.DsOliPointsToGraphs
import event.DsOliEvents
import extlogger.DsOliLogger
import event.DsOliMWEvent
import event.DsOliVLSEvent
import boxcalculation.DsOliBoxStep
import pointstograph.DsOliGraph
import boxcalculation.DsOliBoxesCreator
import pointstograph.DsOliVertexMemory
import scala.collection.mutable.ListBuffer
import boxcalculation.DsOliCell
import PartialFunction._
import boxcalculation.DsOliCycle.CycleId
import boxcalculation.DsOliCycle
import scala.collection.mutable.HashMap
import boxcalculation.DsOliBox
import scala.collection.mutable.Set
import entrypoint.Feature._
import pointstograph.DsOliDiEdge
import pointstograph.DsOliVertex
import scala.util.control.Breaks._
import event.DsOliEvent
import event.DsOliVLSEvent
import event.DsOliBTEntryEvent
import event.DsOliOTEntryEvent
import event.DsOliOTExitEvent
import event.DsOliCommentEvent
import event.DsOliFreeEvent
import event.DsOliBTExitEvent
import event.DsOliVLSEvent
import event.DsOliVLSEvent
import event.DsOliFreeEvent
import event.DsOliVLSEvent
import event.DsOliFreeEvent
import event.DsOliVLSEvent
import util.DsOliAddressUtils
import event.DsOliMWEvent
import pointstograph.DsOliVertex
import pointstograph.DsOliType
import event.DsOliMWEvent
import event.DsOliArtificialUndefEvent
import event.DsOliArtificialFreeEvent
import util.DsOliEventUtils
import test.DsOliTestMethods
import pointstograph.ITypeDB

/**
 * @author DSI
 *
 * @constructor creates an EPT creator
 * @param events the event trace
 * @param ptgs the points-to graphs
 * @param boxes the strand sets
 * @param boxesCreator the class for creating the strands
 * @param typeDB the type DB
 */
class DsOliEntryPointCreator(val events: DsOliEvents, val ptgs: DsOliPointsToGraphs, val boxes: DsOliBoxSteps,
  val boxesCreator: DsOliBoxesCreator, typeDB: ITypeDB) extends IDsOliEntryPointCreator {
  val classSignature = "DsOliEntryPointCreator::"

  // EPT expire after 20 time steps
  val eptTimeOut = 20

  /**
   * Checks, if a cell of the sequence resides inside
   * the given vertex.
   *
   * @param vertex the vertex to check against
   * @param cells the sequence of cells
   * @return Boolean
   */
  def isAnyCellOfSequenceInVertex(vertex: DsOliVertexMemory, cells: ListBuffer[DsOliCell]): Boolean = {
    val funSignature = classSignature + "isAnyCellOfSequenceInVertex:"
    DsOliLogger.debug(funSignature + " entered: vertex = " + vertex + " cells = " + cells)
    if (cells.exists(cell => boxesCreator.regionsOverlapping(vertex.bAddr, vertex.eAddr, cell.bAddr, cell.eAddr))) {
      DsOliLogger.debug(funSignature + " found overlap")
      true
    } else {
      DsOliLogger.debug(funSignature + " found no overlap")
      false
    }
  }

  /**
   * Does there exist a strand running through this vertex
   *
   * @param boxes the set of strands to check
   * @param vertex the vertex to check against
   * @return Boolean
   */
  def targetVertexHasBox(boxes: DsOliBoxStep, vertex: DsOliVertexMemory): Boolean = {
    val funSignature = classSignature + "targetVertexHasBox:"
    DsOliLogger.debug(funSignature + " entered: vertex = " + vertex)

    // Check the linear part of the strands
    ((boxes.boxes.exists { boxTuple =>
      val (boxId, box) = boxTuple
      isAnyCellOfSequenceInVertex(vertex, box.cells)
    })

      ||

      // Check the cyclic parts
      (boxes.cycles.exists { cycleTuple =>
        val (cycleId, cycle) = cycleTuple
        isAnyCellOfSequenceInVertex(vertex, cycle.cells)
      }))
  }

  /**
   * Checks, if the edge (define by source and target address)
   * is an entry pointer
   *
   * @param sourceAddress the source address
   * @param targetAddress the target address
   * @param boxes the set of strands
   * @param ptg the points-to graph
   * @return Option if EP tuple with EP vertex and the offset of the outgoing pointer
   */
  def isEP(sourceAddress: Long, targetAddress: Long, boxes: DsOliBoxStep, ptg: DsOliGraph): Option[(DsOliVertexMemory, Long)] = {
    val funSignature = classSignature + "isEP:"
    DsOliLogger.debug(funSignature + " entered: sourceAddress: " + sourceAddress.toHexString + " targetAddress: " + targetAddress.toHexString)

    // Fetch the vertices for the addresses
    val vertexSOpt = ptg.getVertexForAddress(sourceAddress)
    val vertexTOpt = ptg.getVertexForAddress(targetAddress)

    // Need to skip registers
    if (DsOliAddressUtils.isRegister(sourceAddress)) return None

    if (vertexSOpt.isDefined && vertexTOpt.isDefined) {

      // Fetch the vertices
      val vertexS = ptg.getMemoryVertex(vertexSOpt.get)
      val vertexT = ptg.getMemoryVertex(vertexTOpt.get)

      // Not the same vertex
      val verticesNotEqual = (vertexS != vertexT)

      // No minimum condition between source and target
      val noMinimumCondition = (boxesCreator.minCond(sourceAddress, targetAddress, ptg, boxes).isEmpty)

      // The target vertex needs to see a strand
      val vertexTHasBox = targetVertexHasBox(boxes, vertexT)

      // All conditions need to be fulfilled
      if (verticesNotEqual
        && noMinimumCondition
        && vertexTHasBox) {
        DsOliLogger.debug(funSignature + " found EP")
        return Some((vertexS, sourceAddress - vertexS.bAddr))
      } else {
        DsOliLogger.debug(funSignature + "coonditions are not true: verticesNotEqual = "
          + verticesNotEqual + " noMinimumCondition = " +
          noMinimumCondition + " vertexTHasBox = " + vertexTHasBox)
      }
    } else {
      DsOliLogger.debug(funSignature + "vertexSOpt.isDefined || vertexTOpt.isDefined is false: "
        + vertexSOpt.isDefined + ", " + vertexTOpt.isDefined)
    }
    DsOliLogger.debug(funSignature + " no EP found")
    return None
  }

  /**
   * Retire all EPTs associated with the given
   * entry pointer
   *
   * @param ep EP tuple (EP vertex, offset)
   * @param Fdone the feature set for retired EPTs
   * @param Fbuilding the feature set of life EPTs
   */
  def retireEPTsForCells(ep: (DsOliVertexMemory, Long), Fdone: DsOliFeatureSet, Fbuilding: DsOliFeatureSet): Unit = {
    val funSignature = classSignature + "retireEPTsForCells:"
    DsOliLogger.debug(funSignature + " entered: ep: " + ep + " Fdone: " + Fdone + " Fbuilding: " + Fbuilding)
    val (epVertex, oep) = ep
    val delList = new ListBuffer[DsOliEPT]()

    // Copy features from building to done
    Fbuilding.features.foreach {
      feature =>
        // Features associated with the entry pointer vertex at the
        // given offset will be retired
        if (feature.epVertexId == epVertex.id && feature.oep == oep) {
          DsOliLogger.debug(funSignature + " moving feature from building -> done: " + feature)

          // Mark for removal from Fbuilding
          delList.append(feature)

          // Directly append to Fdone
          Fdone.features.append(feature)
        } else {
          DsOliLogger.debug(funSignature + " feature remains in building: " + feature)
        }
    }

    // Delete moved features from Fbuilding
    delList.foreach(ept => Fbuilding.features -= (ept))

  }

  /**
   * Get all potentially changed EPTs for a given
   * EP
   *
   * @param ep the EP
   * @param Fbuilding the current set of EPTs
   * @return a list of potentially changed EPTs
   */
  def getPotentiallyChanged(ep: (DsOliVertexMemory, Long), Fbuilding: DsOliFeatureSet): ListBuffer[DsOliEPT] = {
    val retEPTs = new ListBuffer[DsOliEPT]()
    val (epVertex, oep) = ep
    val funSignature = classSignature + "getPotentiallyChanged:"
    DsOliLogger.debug(funSignature + " entered: epVertex: " + epVertex + " oep = " + oep + " Fbuilding: " + Fbuilding)

    // Check through all EPTs
    Fbuilding.features.foreach {
      feature =>
        // vertex and pointer offset need to match
        if (feature.epVertexId == epVertex.id && feature.oep == oep) {
          DsOliLogger.debug(funSignature + " found cell to remove: " + feature.Aup)
          retEPTs.append(feature)
        }
    }

    return retEPTs
  }

  /**
   * Checks, if a given strand (defined by the cell sequence,
   * and cycle id) runs through the given vertex.
   *
   * @param vertex the vertex to check
   * @param cellSequence the linear cell sequence of the strand
   * @param cycleId the ID of the cycle connected with the strand (possibly no cycle connected)
   * @param cycles all cycles, used to lookup the cycle by the given cycle id
   * @return Boolean
   */
  def boxRunsThroughVertex(vertex: DsOliVertexMemory, cellSequence: ListBuffer[DsOliCell], cycleId: CycleId, cycles: HashMap[CycleId, DsOliCycle]): Boolean = {
    val funSignature = classSignature + "boxRunsThroughVertex: "
    DsOliLogger.debug(funSignature + "entered")

    // Linear cell sequence
    if (isAnyCellOfSequenceInVertex(vertex, cellSequence)) return true

    // Cyclic cell sequence, if cycle is present
    if (cycleId != 0 && cycles.contains(cycleId)) {
      if (isAnyCellOfSequenceInVertex(vertex, cycles.get(cycleId).get.cells)) return true
    }

    return false
  }

  /**
   * Get the most upstream element of the
   * cell sequence, i.e., the first element
   *
   * @param cells the cells
   * @return the most upstream element
   */
  def mostUpstream(cells: ListBuffer[DsOliCell]): DsOliCell = {
    val funSignature = classSignature + "mostUpstream: "
    DsOliLogger.debug(funSignature + "entered: cells = " + cells)
    cells(0)
  }

  /**
   * Fetch all cells that reside inside of the
   * given vertex and create EPT cells from them
   *
   * @param boxOffset the linkage offset of the strand
   * @param vertex the vertex to check against
   * @param cells the cells to check
   * @return Option list of EPT cells
   */
  def fetchCellsInVertex(boxOffset: Long, vertex: DsOliVertexMemory, cells: ListBuffer[DsOliCell]): Option[ListBuffer[DsOliCell]] = {
    val funSignature = classSignature + "fetchCellsInVertex: "
    DsOliLogger.debug(funSignature + "entered: vertex = " + vertex)

    // First check, that there actually exist cells in this vertex
    return if (isAnyCellOfSequenceInVertex(vertex, cells)) {
      val CtmpSet = new ListBuffer[DsOliCell]()

      cells.foreach(cell =>
        if (boxesCreator.regionsOverlapping(vertex.bAddr, vertex.eAddr, cell.bAddr, cell.eAddr)) {
          DsOliLogger.debug(funSignature + "found cell inside of vertex. cell = " + cell)
          // Create a new EPT cell from the original cell plus the linkage offset and the 
          // position of the EPT cell inside of the vertex
          CtmpSet.append(new DsOliEPTCell(cell, boxOffset, cell.bAddr - vertex.bAddr))
        })

      Some(CtmpSet)
    } else {
      None
    }
  }

  /**
   * Only fetch the supersets of the cell groups
   * to avoid clutter. There is no check for
   * linkage offsets just the maximal coverage
   * achieved by a superset is considered
   *
   * @param groups the list of group sets
   * @return the list of group sets containing only the supersets of cells
   */
  def getSupersetGroups(groups: ListBuffer[DsOliCellGroup]): ListBuffer[DsOliCellGroup] = {
    val funSignature = classSignature + "getSupersetGroups: "
    DsOliLogger.debug(funSignature + "entered: groups.size = " + groups.size)
    val groupSupersets = new ListBuffer[DsOliCellGroup]()

    var i = 0
    // Cycle through all group elements
    for (i <- 0 until groups.length) {
      val element = groups(i)
      val elementSet = (element.cellSeq ++ element.cycleSeq).toSet
      DsOliLogger.debug(funSignature + "outer loop: checking element = " + element)
      // Test if there already was recorded a superset for this group
      if (!groupSupersets.exists {
        group =>
          val existCmpSet = (group.cellSeq ++ group.cycleSeq).toSet
          DsOliLogger.debug(funSignature + "exists: checking against group = " + group)
          DsOliLogger.debug(funSignature + "\telementSet.subsetOf(existCmpSet) = " + elementSet.subsetOf(existCmpSet))
          elementSet.subsetOf(existCmpSet)
      }) {
        // No superset recorded yet: test the rest of the elements
        var curSuperset = elementSet
        var curElement = element

        // Cycle through all elements after the current one
        for (e <- i + 1 until groups.length) {
          val cmpElement = groups(e)
          val cmpElementSet = (cmpElement.cellSeq ++ cmpElement.cycleSeq).toSet
          DsOliLogger.debug(funSignature + "inner loop: checking cmpElement = " + cmpElement)
          // Test if the current superset is the subset of the current element
          // under test. If this is the case, we found a new superset
          if (curSuperset.subsetOf(cmpElementSet)) {
            DsOliLogger.debug(funSignature + "inner loop: found new superset")
            curSuperset = cmpElementSet
            curElement = cmpElement
          } else {
            DsOliLogger.debug(funSignature + "inner loop: no new superset found")
          }
        }

        DsOliLogger.debug(funSignature + "outer loop: result of inner loop (superset) curElement = " + curElement)
        // Add the actual group element to the set of found supersets
        groupSupersets.append(curElement)

      }

    }

    // Debug
    DsOliLogger.debug(funSignature + "groupSupersets.size = " + groupSupersets.size)
    groupSupersets.foreach {
      group =>
        DsOliLogger.debug(funSignature + "\tgroup: " + group)
        DsOliLogger.debug(funSignature + "\tgroup cellSeq: " + group.cellSeq)
        DsOliLogger.debug(funSignature + "\tgroup cycleSeq: " + group.cycleSeq)
    }

    groupSupersets
  }

  /**
   * Fetch cell groups for the given vertex.
   * The groups are filtered to only contain
   * the supersets of cells, to avoid clutter.
   * Linkage offsets are not considered in
   * any way to distinguish between
   * cells of different strands.
   *
   * @param vertex the vertex to test
   * @param boxes the set of strands
   * @return list of cell groups
   */
  def getAllCellGroups(vertex: DsOliVertexMemory, boxes: DsOliBoxStep): ListBuffer[DsOliCellGroup] = {
    val funSignature = classSignature + "getAllCellGroups: "
    DsOliLogger.debug(funSignature + "entered: vertex = " + vertex)

    // Stores strands for through the vertex
    val Bv = new ListBuffer[DsOliBox]()

    // Fetch all strands inside of vertex
    boxes.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        if (boxRunsThroughVertex(vertex, box.cells, box.cycleId, boxes.cycles)) {
          DsOliLogger.debug(funSignature + "Box runs through vertex. Box = " + box)
          Bv.append(box)
        }
    }

    // Set of cell groups
    val Cset = new ListBuffer[DsOliCellGroup]()

    // Fetch all cells per box that are inside of vertex
    Bv.foreach {
      box =>
        // Test the sequence
        val CtmpSeqOpt = fetchCellsInVertex(box.offset, vertex, box.cells)

        // Test the cycle
        val CtmpCycleOpt = if (box.cycleId != 0 && boxes.cycles.contains(box.cycleId)) {
          val cycleCells = boxes.cycles.get(box.cycleId).get.cells
          fetchCellsInVertex(box.offset, vertex, cycleCells)
        } else {
          None
        }

        // Create new entry if either of the two sets is defined
        if (CtmpSeqOpt.isDefined || CtmpCycleOpt.isDefined) {
          DsOliLogger.debug(funSignature + "at least one set is defined. Appending to return set.")
          Cset.append(new DsOliCellGroup(CtmpSeqOpt, CtmpCycleOpt))
        }

    }

    // Only select the supersets to get the minimal coverage of all cells

    return getSupersetGroups(Cset)
  }

  /**
   * Get the most upstream cell for each
   * cell group superset found in the
   * vertex
   *
   * @param vertex the vertex to check against
   * @param boxes the set of strands to check through
   * @return a list of all the most upstream cells
   */
  def getAllMostUpstreamCells(vertex: DsOliVertexMemory, boxes: DsOliBoxStep): ListBuffer[DsOliCell] = {
    val funSignature = classSignature + "getAllMostUpstreamCells: "

    DsOliLogger.debug(funSignature + " entered: vertex = " + vertex)

    val retList = new ListBuffer[DsOliCell]()

    // Set of cell groups (supersets)
    val Cset = getAllCellGroups(vertex, boxes)

    // Check through each cell group
    Cset.foreach {
      group =>

        // A group consists of the sequential
        // and cyclic sequence
        val seqGroup = group.cellSeq
        val cycleGroup = group.cycleSeq

        // Per definition: If there exists a cell which is part of the sequential cells
        // it is chosen as the most upstream cell.
        if (group.cellSeq.isDefined) {
          DsOliLogger.debug(funSignature + "found sequential cell. cell = " + group.cellSeq.get(0))
          retList.append(group.cellSeq.get(0))
          // If no sequential part is present, then choose the first cell of the cyclic 
          // part as the most upstream cell 
        } else if (group.cycleSeq.isDefined) {
          DsOliLogger.debug(funSignature + "found cyclic cell. cell = " + group.cycleSeq.get(0))
          retList.append(group.cycleSeq.get(0))
        } else {
          // Debugging only
          DsOliLogger.warning(funSignature + "found neither sequential nor cyclic cell.")
        }
    }
    retList
  }

  /**
   * Fetches all strand IDs of strands
   * which contain the given EPT cell.
   * The linkage offset of the EPT
   * cell is considered.
   *
   * @param cell the cell to search for
   * @param boxes the strands to search in
   * @return returns a set of strand ids
   */
  def boxSet(cell: DsOliCell, boxes: DsOliBoxStep): Set[Long] = {
    val funSignature = classSignature + "boxSet: "
    val cellLinkageOffset = cell.asInstanceOf[DsOliEPTCell].linkageOffset
    DsOliLogger.debug(funSignature + "entered. cellLinkageOffset = " + cellLinkageOffset)
    val retSet = Set[Long]()

    // Check all strands for the given cell
    // taking the linkage offset into consideration
    boxes.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple
        if (box.offset == cellLinkageOffset &&
          ( // Test linear
            box.cells.contains(cell)
            // Test cyclic
            || box.cycleId != 0 && boxes.cycles.get(box.cycleId).get.cells.contains(cell))) {
          DsOliLogger.debug(funSignature + "found box. offset = " + box.offset + " id = " + box.id)
          retSet += box.id
        }
    }
    retSet
  }

  /**
   * Checks, if all cells in the vertex are connected
   * to a strand which was seen previously. Effectively
   * testing, that the strands where split apart due to
   * an upstream movement of the EP, but they still all
   * reside in the same vertex.
   *
   * @param cell the cell we are currently checking
   * @param cells the most upstream cells found in the vertex
   * @param boxIdSetOld the old set of strands seen by the EP previously
   * @param boxIdSetNew the new set of strands seen by the EP currently
   * @param boxes the strand set
   * @return Boolean
   */
  def isNotABoxSplit(cell: DsOliCell, cells: ListBuffer[DsOliCell], boxIdSetOld: Set[Long], boxIdSetNew: Set[Long], boxes: DsOliBoxStep): Boolean = {
    val funSignature = classSignature + "isNotABoxSplit: "
    DsOliLogger.debug(funSignature + "entered")

    // Check through all upstream cells
    !cells.exists {
      cellPrime =>
        val boxIdSetPrime = boxSet(cellPrime, boxes)
        // Self check & intersection between the two sets
        cell != cellPrime && (boxIdSetPrime.&((boxIdSetOld.diff(boxIdSetNew))).size != 0)
    }

  }

  /**
   * Calculates the offset of the cell inside of the vertex
   *
   * @param vertex the surrounding vertex
   * @param cell the cell inside of the vertex
   * @return the offset of the start of the cell from the start of the vertex
   */
  def cellOffset(vertex: DsOliVertexMemory, cell: DsOliCell): Long = {
    val funSignature = classSignature + "cellOffset: "
    DsOliLogger.debug(funSignature + "entered")
    return cell.bAddr - vertex.bAddr
  }

  /**
   * Fetch the first strand which runs through
   * the vertex and has the linkage offset of
   * the cell
   *
   * @param cell the EPT cell
   * @param vertex the vertex to check against
   * @param boxes the strand set to search in
   * @return Option the chosen strand
   */
  def nonDeterministicBoxSelect(cell: DsOliCell, vertex: DsOliVertexMemory, boxes: DsOliBoxStep): Option[DsOliBox] = {
    val funSignature = classSignature + "nonDeterministicBoxSelection: "
    DsOliLogger.debug(funSignature + "entered: cell = " + cell)
    DsOliLogger.debug(funSignature + "\tcell.linkageOffset = " + cell.asInstanceOf[DsOliEPTCell].linkageOffset)
    val linkageOffset = cell.asInstanceOf[DsOliEPTCell].linkageOffset
    var retBox: Option[DsOliBox] = None

    // Check through the strands
    boxes.boxes.exists {
      boxTuple =>
        val (boxId, box) = boxTuple
        DsOliLogger.debug(funSignature + "Testing box : " + box)
        // Check the linkage offset and that the strand runs through the vertex
        if (box.offset == linkageOffset && boxRunsThroughVertex(vertex, box.cells, box.cycleId, boxes.cycles)) {
          DsOliLogger.debug(funSignature + "found box: box.id = " + box.id)
          retBox = Some(box)
          true
        } else {
          DsOliLogger.debug(funSignature + "Can not use box : " + box)
          false
        }
    }
    retBox
  }

  /**
   * Fetch the downstream strand sequence
   * inside of a cycle.
   *
   * @param cell the cell where to start
   * @param cycle the cycle to cut
   * @return the downstream cell sequence
   */
  def dsCycleSeq(cell: DsOliCell, cycle: DsOliCycle): ListBuffer[DsOliCell] = {
    val funSignature = classSignature + "dsCycleSeq: "
    DsOliLogger.debug(funSignature + "entered")
    val cellIndex = cycle.cells.indexOf(cell)
    val retSeq = new ListBuffer[DsOliCell]()
    DsOliLogger.debug(funSignature + "cellIndex = " + cellIndex)
    if (cellIndex >= 0) {
      retSeq.append(cell)
      for (i <- cellIndex + 1 until cycle.cells.length) {
        DsOliLogger.debug(funSignature + "iteration: cellIndex = " + i)
        retSeq.append(cycle.cells(i % cycle.cells.length))
      }
    }
    retSeq
  }

  /**
   * Get the downstream sequence of the
   * given strand starting at the
   * given cell.
   *
   * @param cell the cell where to start
   * @param box the strand
   * @param boxes the strand set, used for fetching cycles
   * @return the downstream cell sequence
   */
  def dsSeq(cell: DsOliCell, box: DsOliBox, boxes: DsOliBoxStep): ListBuffer[DsOliCell] = {
    val funSignature = classSignature + "dsSeq: "
    val retSeq = new ListBuffer[DsOliCell]()
    DsOliLogger.debug(funSignature + "entered")

    // Process sequence
    val cellIndex = box.cells.indexOf(cell)
    if (cellIndex >= 0) {
      DsOliLogger.debug(funSignature + "found match on cellIndex = " + cellIndex)
      for (i <- cellIndex until box.cells.length) {
        DsOliLogger.debug(funSignature + "appending cell")
        retSeq.append(box.cells(i))
      }
    }

    // Process cycle
    if (box.cycleId != 0 && boxes.cycles.contains(box.cycleId)) {
      val cycle = boxes.cycles.get(box.cycleId).get
      // No linear part: add complete cycle
      if (retSeq.isEmpty) {
        retSeq.appendAll(dsCycleSeq(cycle.cells.head, cycle))
      } else if (!retSeq.isEmpty) {
        // Linear part present: start from cell
        val cellOpt = cycle.getCellById(box.cycleEntryPoint)
        if (cellOpt.isDefined) {
          val cell = cellOpt.get
          retSeq.appendAll(dsCycleSeq(cell, cycle))
        }
      }
    }

    retSeq
  }

  /**
   * Calculates changes of the observed strands from
   * the point of view of the EP.
   *
   * @param vertex the vertex the EP is currently pointing at
   * @param A1 the old most upstream cell associated with the EP
   * @param boxesIMinusOne the strand set of the previous time step (i-1)
   * @param A2 the current most upstream cell fulfilling the EPT reuse condition
   * @param boxesI the strand set of the current time step (i)
   * @return the calculate feature representing the changes seen by the EP
   */
  def calculateFeature(vertex: DsOliVertexMemory, A1: DsOliCell, boxesIMinusOne: DsOliBoxStep, A2: DsOliCell, boxesI: DsOliBoxStep): Feature = {
    val funSignature = classSignature + "calculateFeature: "

    DsOliLogger.debug(funSignature + "entered: " + vertex)
    DsOliLogger.debug(funSignature + "A1: " + A1)
    DsOliLogger.debug(funSignature + "A2: " + A2)

    // Fetch one box for previous and current time step
    val boxA1Opt = nonDeterministicBoxSelect(A1, vertex, boxesIMinusOne)
    val boxA2Opt = nonDeterministicBoxSelect(A2, vertex, boxesI)

    // Sanity
    if (boxA1Opt.isEmpty || boxA2Opt.isEmpty) {
      DsOliLogger.debug(funSignature + "boxesIMinusOne: " + boxesIMinusOne)
      DsOliLogger.debug(funSignature + "boxesI: " + boxesI)
      throw new Exception(funSignature + "boxA1 || boxA2 undefined")
    }

    // Fetch the downstream sequences
    val lenA1 = dsSeq(A1, boxA1Opt.get, boxesIMinusOne).length
    val lenA2 = dsSeq(A2, boxA2Opt.get, boxesI).length

    DsOliLogger.debug(funSignature + "lenA1 = " + lenA1 + "; lenA2 = " + lenA2)

    // Calculate the feature
    if (lenA1 == lenA2) return Feature.noChange
    if (lenA1 + 1 == lenA2) return Feature.plusOne
    if (lenA1 - 1 == lenA2) return Feature.minusOne
    if (lenA1 < lenA2) return Feature.plusN

    return Feature.minusN
  }

  /**
   * Calculate the changes due to an EP write
   *
   * @param targetVertex the target vertex of the pointer write
   * @param ep the EP
   * @param boxesIMinusOne the strand set at the previous time step (i-1)
   * @param boxesI the strand set of the current time step (i)
   * @param i the time step
   * @param Fbuilding the feature set of live EPTs
   * @param Fdone the feature set of retired EPTs
   */
  def calculateEPWriteChanges(targetVertex: DsOliVertexMemory, ep: (DsOliVertexMemory, Long),
    boxesIMinusOne: DsOliBoxStep, boxesI: DsOliBoxStep, i: Int, Fbuilding: DsOliFeatureSet, Fdone: DsOliFeatureSet): Unit = {
    val funSignature = classSignature + "calculateEPWriteChanges: "

    // Get all potentially changed EPTs from the current feature set
    val FptChanged = getPotentiallyChanged(ep, Fbuilding)

    // Fetch all most upstream cells of boxes through the target vertex
    val CupSet = getAllMostUpstreamCells(targetVertex, boxesI)

    // Sets for recording deletion candidates
    val delCupSet = new ListBuffer[DsOliCell]()
    val delFptChanged = new ListBuffer[DsOliEPT]()

    // Split the EP into the vertex and the EP offset
    val (epVertex, oep) = ep

    DsOliLogger.debug(funSignature + "entered: targetVertex = " + targetVertex + " ep = " + ep)

    // Cycle through the potentially changed EPTs and
    // see if they actually changed
    FptChanged.foreach { ept =>

      // Fetch all strands which ran through the EPT cell
      // in the previous time step
      val boxIdSetOld = boxSet(ept.Aup, boxesIMinusOne)

      // Most upstream cell, which fulfills the condition for reuse
      var lastCell: DsOliCell = null
      if (CupSet.exists {
        cell =>
          val boxIdSetNew = boxSet(cell, boxesI)
          lastCell = cell
          // First check: old and new strand sets are equal and new elements in new strand set
          (boxIdSetOld.subsetOf(boxIdSetNew) ||
            // Second check: ept moved upstream with box split (new set is subset of old set) but 
            // previously seen strands are still present in the vertex (though now split)
            // and no new strands are present in the vertex
            ((boxIdSetNew.subsetOf(boxIdSetOld) && (boxIdSetOld.diff(boxIdSetNew).size != 0) &&
              isNotABoxSplit(cell, CupSet, boxIdSetOld, boxIdSetNew, boxesI))) ||
              // Third check: ept moved upstream with box split (new set is subset of old set) and
              // at least one cell stayed at the same offset
              ((boxIdSetNew.subsetOf(boxIdSetOld) && (boxIdSetOld.diff(boxIdSetNew).size != 0) &&
                cellOffset(targetVertex, cell) == ept.Aup.asInstanceOf[DsOliEPTCell].cellOffset)))
      }) {

        // Calculate the feature
        DsOliLogger.debug(funSignature + "condition for epw reuse is met.")
        val feature = calculateFeature(targetVertex, ept.Aup, boxesIMinusOne, lastCell, boxesI)
        DsOliLogger.debug(funSignature + "calculated feature = " + feature)

        // Update the EPT
        ept.Aup = lastCell.asInstanceOf[DsOliEPTCell]

        // Append the new feature
        ept.Qf.append(feature)

        // Bookkeeping
        delCupSet.append(lastCell)
        delFptChanged.append(ept)
      }
    }

    // Remove all processed cells from the CupSet
    delCupSet.foreach {
      cell =>
        CupSet -= cell
        DsOliLogger.debug(funSignature + "deleting from CupSet cell = " + cell)
    }

    // For all unprocessed cells in CupSet a new ept needs to be created
    CupSet.foreach {
      cell =>
        val newEpt = new DsOliEPT(epVertex, epVertex.id, oep, cell.asInstanceOf[DsOliEPTCell], i, new ListBuffer[Feature]())
        newEpt.Qf.append(Feature.newlyCreated)
        Fbuilding.features.append(newEpt)
        DsOliLogger.debug(funSignature + "created new ept = " + newEpt)

    }

    // Remove the processed epts from FptChanged
    delFptChanged.foreach {
      ept =>
        DsOliLogger.debug(funSignature + "deleting from FptChanged ept = " + ept)
        FptChanged.-=(ept)
    }

    // All epts remaining in FptChanged were not processed.
    // They are moved to Fdone and get removed from Fbuilding
    FptChanged.foreach {
      ept =>
        DsOliLogger.debug(funSignature + "moving to done. ept = " + ept)
        Fdone.features.append(ept)

        DsOliLogger.debug(funSignature + "deleting from Fbuilding ept = " + ept)
        Fbuilding.features -= ept
    }
  }

  /**
   * Fetch the feature representation for the event
   *
   * @param event the event to represent as a feature
   * @return the Feature for the event
   */
  def getUpdateFeatureForEvent(event: DsOliEvent): Feature = {
    val funSignature = classSignature + "getUpdateFeatureForEvent: "
    DsOliLogger.debug(funSignature + "entered: ")
    event match {
      case e: DsOliBTEntryEvent => btEntryNoChange
      case e: DsOliBTExitEvent => btExitNoChange
      case e: DsOliFreeEvent => freeNoChange
      case e: DsOliCommentEvent => commentNoChange
      case e: DsOliOTEntryEvent => otEntryNoChange
      case e: DsOliOTExitEvent => otExitNoChange
      case e: DsOliVLSEvent => vlsNoChange
      case _ => noChange // DsOliMWEvent 
    }
  }

  /**
   * Update all EPTs, which were not updated thus far
   * in the current time step.
   *
   * @param i the time step
   * @param e the event that caused the change
   * @param Fbuilding the set of life EPTs
   */
  def updateUnchangedEPTs(i: Int, e: DsOliEvent, Fbuilding: DsOliFeatureSet): Unit = {
    val funSignature = classSignature + "updateUnchangedEPTs: "
    DsOliLogger.debug(funSignature + "entered: Time step i = " + i)

    // Check all life EPTs and update their features,
    // if they have not been processed before in this
    // time step.
    Fbuilding.features.foreach {
      feature =>

        // The length of the feature trace shows, if the 
        // EPT was updated in this time step
        if (feature.creationTime + feature.Qf.size - 1 < i) {
          DsOliLogger.debug(funSignature + "found noChange candidate: feature = " + feature)
          DsOliLogger.debug(funSignature + " -> feature.Qf.size = " + feature.Qf.size)
          // Create and record the no change event
          feature.Qf.append(getUpdateFeatureForEvent(e))
        } else {
          DsOliLogger.debug(funSignature + "no change for feature = " + feature)
          DsOliLogger.debug(funSignature + "\tfeature.creationTime(" + feature.creationTime + ") + feature.Qf.size(" +
            feature.Qf.size + ") - 1 < i(" + i + ") = " + (feature.creationTime + feature.Qf.size - 1 < i))
        }
    }
  }

  /**
   * Return the set of groups that contain the cell
   *
   * @param vertex the vertex to fetch the cell groups for
   * @param cell the cell to search for in the cell groups for the vertex
   * @param boxesI strands of the current time step (i)
   * @return list of cell groups containing the cell
   */
  def getCellGroups(vertex: DsOliVertexMemory, cell: DsOliCell, boxesI: DsOliBoxStep): ListBuffer[DsOliCellGroup] = {
    val funSignature = classSignature + "getCellGroups: "
    DsOliLogger.debug(funSignature + "entered")

    // First fetch all cell groups
    val allCellGroups = getAllCellGroups(vertex, boxesI)
    val retCellGroups = new ListBuffer[DsOliCellGroup]()

    // Pick cell groups which contain the cell
    allCellGroups.foreach {
      cellGroup =>
        // Check linear sequence
        if ((cellGroup.cellSeq.isDefined && cellGroup.cellSeq.get.exists(iterCell => cell == iterCell)) ||
          // Check cyclic sequence
          (cellGroup.cycleSeq.isDefined && cellGroup.cycleSeq.get.exists(iterCell => cell == iterCell))) {
          DsOliLogger.debug(funSignature + "found cell in sequence or cycle")
          retCellGroups.append(cellGroup)
        }
    }
    retCellGroups
  }

  /**
   * Return all entry pointer tags for the given vertex
   *
   * @param vertexForCell the vertex to consider
   * @param boxesI the strand set of the current time step (i)
   * @param ptgI the points-to graph of the current time step (i)
   * @return list of EPs and offset
   */
  def calculateEPs(vertexForCell: DsOliVertexMemory,
    boxesI: DsOliBoxStep, ptgI: DsOliGraph): ListBuffer[(DsOliVertexMemory, Long)] = {
    val funSignature = classSignature + "calculateEPs: "

    DsOliLogger.debug(funSignature + "entered")

    val retEPs = new ListBuffer[(DsOliVertexMemory, Long)]()
    val epEdges = new ListBuffer[DsOliDiEdge[DsOliVertex]]()
    val iter = ptgI.graph.edges.iterator

    // Fetch all edges that are entry pointers
    while (iter.hasNext) {
      val edgeOuter = iter.next.toOuter

      // Edge points into given vertex
      if (boxesCreator.regionsOverlapping(vertexForCell.bAddr, vertexForCell.eAddr,
        edgeOuter.tAddr + edgeOuter.tOffset, edgeOuter.tAddr + edgeOuter.tOffset)) {

        DsOliLogger.debug(funSignature + "potential ep found: " + edgeOuter)

        // Check if edge is entry pointer
        val ep = isEP(edgeOuter.sAddr + edgeOuter.sOffset, edgeOuter.tAddr + edgeOuter.tOffset, boxesI, ptgI)
        if (ep.isDefined) {
          DsOliLogger.debug(funSignature + "ep is defined on edge = " + edgeOuter)
          epEdges.append(edgeOuter)
          val vertexS = ptgI.getMemoryVertex(ptgI.getVertexForAddress(edgeOuter.sAddr).get)
          retEPs.append((vertexS, edgeOuter.sAddr + edgeOuter.sOffset - vertexS.bAddr))
        } else {
          DsOliLogger.debug(funSignature + "no ep found")
        }
      }
    }

    DsOliLogger.debug(funSignature + "finished selecting relevant eps.")

    retEPs
  }

  /**
   * Get the features associated with
   * the given entry pointer
   *
   * @param ep tuple of the entry pointer vertex and the entry pointer offset
   * @param featureSet the set of entry pointer tags to search in
   * @returns a list of entry pointer tags
   */
  def getFeatureSubsetByEP(ep: (DsOliVertexMemory, Long), featureSet: DsOliFeatureSet): ListBuffer[DsOliEPT] = {
    val funSignature = classSignature + "getFeatureSubsetByEP: "
    DsOliLogger.debug(funSignature + "entered")
    val retSet = new ListBuffer[DsOliEPT]()
    featureSet.features.foreach {
      feature =>
        // Feature is associated with entry pointer vertex and pointer offset match
        if (feature.epVertexId == ep._1.id && feature.oep == ep._2) {
          DsOliLogger.debug(funSignature + "found feature = " + feature)
          retSet.append(feature)
        }
    }
    retSet
  }

  /**
   * Fetches the first EPT, that contains the
   * given cell and is associated with the
   * given entry pointer.
   *
   * @param ep tuple of the entry pointer vertex and the entry pointer offset
   * @param cell the EPT cell to check for
   * @param featureSet the set of EPTs to search in
   */
  def featureSetContainsEPAndCell(ep: (DsOliVertexMemory, Long), cell: DsOliCell, featureSet: ListBuffer[DsOliEPT]): Option[DsOliEPT] = {
    val funSignature = classSignature + "featureSetContainsEPAndCell: "
    DsOliLogger.debug(funSignature + "entered")
    val (epVertex, oep) = ep
    val eptCell = cell.asInstanceOf[DsOliEPTCell]
    featureSet.foreach {
      feature =>
        if (feature.epVertexId == epVertex.id && feature.oep == oep &&
          feature.Aup == eptCell) {
          DsOliLogger.debug(funSignature + "found feature = " + feature)
          return Some(feature)
        }
    }
    DsOliLogger.debug(funSignature + "no feature found")
    None
  }

  /**
   * Retire all cells by moving them from the current
   * life set of EPTs to the set of unused EPTs.
   *
   * @param group the cells to retire
   * @param Fdone the set of retired EPTs
   * @param Fbuilding the set of life EPTs
   */
  def retireEPTsForCells(group: DsOliCellGroup, Fdone: DsOliFeatureSet, Fbuilding: DsOliFeatureSet): Unit = {
    val funSignature = classSignature + "retireEPTsForCells: "
    DsOliLogger.debug(funSignature + "entered")

    // Check through all life EPTs
    Fbuilding.features.foreach {
      ept =>

        // Does there exist a cell in the group corresponding to the EPT?
        // If yes, retire.
        if ((group.cellSeq.isDefined && group.cellSeq.get.contains(ept.Aup)) ||
          (group.cycleSeq.isDefined && group.cycleSeq.get.contains(ept.Aup))) {
          DsOliLogger.debug(funSignature + "moving ept from building to done. ept = " + ept)

          // Move from Fbuilding to Fdone
          Fdone.features.append(ept)
          Fbuilding.features -= ept
        }
    }
  }

  /**
   * Update all EPTs that have not bee changed
   * yet during the current time step.
   *
   * @param boxesIMinusOne the set of strands of the previous time step (i-1)
   * @param boxeI the set of strands of the current time step (i)
   * @param ptg the current points-to graph
   * @param Fbuildin the set of life EPTs
   * @param i the time step
   */
  def updateAllOtherEPTs(boxesIMinusOne: DsOliBoxStep, boxesI: DsOliBoxStep, ptg: DsOliGraph,
    Fbuilding: DsOliFeatureSet, i: Int): Unit = {
    val funSignature = classSignature + "updateAllOtherEPTs: "
    val delBoxIds = boxesIMinusOne.boxes.keySet.diff(boxesI.boxes.keySet)
    DsOliLogger.debug(funSignature + "entered")

    // Check through all EPTs and update the unchanged
    Fbuilding.features.foreach {
      ept =>
        val boxIds = boxSet(ept.Aup, boxesIMinusOne)
        // Make sure, that we only process epts, that have not been changed during the
        // current time step
        if (ept.creationTime + ept.Qf.size - 1 < i && delBoxIds.&(boxIds).size != 0) {
          DsOliLogger.debug(funSignature + "found ept to update: ept = " + ept)

          // Observe the changes between the two time steps from the 
          // point of view of the EPT 
          val feature = calculateFeature(ptg.getMemoryVertex(ptg.getVertexForAddress(ept.Aup.bAddr).get),
            ept.Aup, boxesIMinusOne, ept.Aup, boxesI)

          // Record the changes
          ept.Qf.append(feature)

        } else {
          DsOliLogger.debug(funSignature + "no update for ept = " + ept)
          DsOliLogger.debug(funSignature + "\tept.creationTime (" + ept.creationTime + ") + ept.Qf.size (" +
            ept.Qf.size + ") - 1< i (" + i + ") = " + (ept.creationTime + ept.Qf.size - 1 < i))
          DsOliLogger.debug(funSignature + "\tdelBoxIds.&(boxIds).size != 0 = " + (delBoxIds.&(boxIds).size != 0))
        }
    }

  }

  /**
   * Checks, if there exists an EPT for the given cell.
   * The cell is expected to come from a sequence of
   * cells, which are starting from the most upstream
   * part. Therefore the first found EPT for the cell
   * will be the most upstream and therefore can be
   * used to calculate the changes and make the
   * EPT the new most upstream.
   *
   * @param cell the cell of the cell group
   * @param ep tuple with entry pointer and ep offset
   * @param FsubBuilding the current active list
   * @param vertexForCell the vertex holding the cell (group)
   * @param boxesIMinusOne strand set of the previous time step (i-1)
   * @param boxesI strand set of the current time step (i)
   * @param Aup most upstream cell of the cell group
   * @return Boolean whether the EPT was moved upstream
   */
  def moveEPMostUpstreamInGroup(cell: DsOliCell, ep: (DsOliVertexMemory, Long), FsubBuilding: ListBuffer[DsOliEPT],
    vertexForCell: DsOliVertexMemory, boxesIMinusOne: DsOliBoxStep,
    boxesI: DsOliBoxStep, Aup: DsOliCell): Boolean = {
    val funSignature = classSignature + "moveEPMostUpstreamInGroup: "

    // First check, if there exists an EPT with this cell
    val eptOpt = featureSetContainsEPAndCell(ep, cell, FsubBuilding)
    if (eptOpt.isDefined) {
      // Calculate the feature
      DsOliLogger.debug(funSignature + "calculate feature")
      val feature = calculateFeature(vertexForCell, cell, boxesIMinusOne, Aup, boxesI)

      // Move the EPT upstream and set the feature 
      eptOpt.get.Aup = Aup.asInstanceOf[DsOliEPTCell]
      eptOpt.get.Qf.append(feature)
      true
    } else {
      false
    }
  }

  /**
   * Update the EPTs for a vertex, resulting
   * in a pointer write that changed the
   * strands.
   *
   * @param cell the EPT cell to consider
   * @param boxesIMinusOne strand set of the previous time step (i-1)
   * @param boxesI strand set of the current time step (i)
   * @param ptgI points-to graph of the current item step (i)
   * @param Fdone set of retired EPTs
   * @param Fbuilding set of life EPTs
   */
  def updateEPTsForCellGroups(cell: DsOliCell, boxesIMinusOne: DsOliBoxStep,
    boxesI: DsOliBoxStep, ptgI: DsOliGraph, i: Int, Fdone: DsOliFeatureSet, Fbuilding: DsOliFeatureSet): Unit = {
    val funSignature = classSignature + "updateEPTsForCellGroups: "
    DsOliLogger.debug(funSignature + "entered: cell = " + cell)

    // Fetch the vertex for the cell
    val vertexForCell = ptgI.getMemoryVertex(ptgI.getVertexForAddress(cell.bAddr).get)
    DsOliLogger.debug(funSignature + "got vertex for cell: vertexForCell = " + vertexForCell)

    // Fetch the cell groups containing the cell
    val cellGroups = getCellGroups(vertexForCell, cell, boxesI)
    DsOliLogger.debug(funSignature + "got cellGroups: cellGroups.size = " + cellGroups.size)

    // Fetch the entry pointer pointing to the vertex
    val eps = calculateEPs(vertexForCell, boxesI, ptgI)
    DsOliLogger.debug(funSignature + "got eps")

    // Process the cell groups
    cellGroups.foreach {
      group =>
        DsOliLogger.debug(funSignature + "processing group")

        // Priority: linear comes first, then cyclic
        // cyclic part is always further downstream 
        // by its nature, i.e., there can never be
        // a linear part leading out of a cycle.
        val Aup = if (group.cellSeq.isDefined) {
          DsOliLogger.debug(funSignature + "most upstream on sequence")
          mostUpstream(group.cellSeq.get)
        } else if (group.cycleSeq.isDefined) {
          DsOliLogger.debug(funSignature + "most upstream on cycle")
          mostUpstream(group.cycleSeq.get)
        } else {
          throw new Exception(funSignature + "cell and cycle seq not defined!")
        }

        // Process all EPs
        // If one needs eps processed in order then insert: .sortBy(_._1.id ).
        eps.foreach {
          ep =>
            DsOliLogger.debug(funSignature + "processing ep: " + ep)

            // Get the set of features associated with the entry pointer
            val FsubBuilding = getFeatureSubsetByEP(ep, Fbuilding)

            // Does there exist an EPT inside the feature set, that 
            // already contains the current most upstream cell
            val eptOpt = featureSetContainsEPAndCell(ep, Aup, FsubBuilding)
            if (eptOpt.isDefined) {
              DsOliLogger.debug(funSignature + "found ep to calculate feature")
              val feature = calculateFeature(vertexForCell, Aup, boxesIMinusOne, Aup, boxesI)
              eptOpt.get.Qf.append(feature)
            } else {
              // There does not exist an EPT with Aup, now check, if there is another EPT
              // which contains a cell of the cell group

              // The if-part does its work directly in the moveEPMostUpstreamInCellGroup
              // method where the cell group check is done. In case there is a match
              // the EPT is set to Aup, i.e., the EPT is moved most upstream 
              if (FsubBuilding.length != 0 &&
                // Test on sequence
                ((group.cellSeq.isDefined && group.cellSeq.get.exists {
                  cell => moveEPMostUpstreamInGroup(cell, ep, FsubBuilding, vertexForCell, boxesIMinusOne, boxesI, Aup)
                })
                  // Or on cycle
                  || (group.cycleSeq.isDefined && group.cycleSeq.get.exists {
                    cell => moveEPMostUpstreamInGroup(cell, ep, FsubBuilding, vertexForCell, boxesIMinusOne, boxesI, Aup)
                  }))) {
                DsOliLogger.debug(funSignature + "moved ep most upstream in group")

              } else {
                // There was no EPT for any cell of the cell group.
                // Create a new EPT with the Aup (most upstream cell of group).
                // This happens, e.g., when a strand comes into existence inside
                // of a vertex, where there was no strand before.
                DsOliLogger.debug(funSignature + "calculate feature")
                val Qf = new ListBuffer[Feature]()
                Qf.append(newlyCreated)
                Fbuilding.features.append(new DsOliEPT(ep._1, ep._1.id, ep._2, Aup.asInstanceOf[DsOliEPTCell], i, Qf))
              }
            }

        }

        // Remove the most upstream cell from the group.
        // Then retire all cells of the group.
        if (group.cellSeq.isDefined) group.cellSeq.get -= Aup
        if (group.cycleSeq.isDefined) group.cycleSeq.get -= Aup
        retireEPTsForCells(group, Fdone, Fbuilding)

    }
  }

  /**
   * Fetch the source EPT cell, if the source and target
   * fulfill a linkage condition
   *
   * @param e the memory write event
   * @param boxesI the strand set of the current time step (i)
   * @param ptgI the points-to graph of the current time step (i)
   * @return Option the EPT cell
   *
   */
  def getCell(e: DsOliMWEvent, boxesI: DsOliBoxStep, ptgI: DsOliGraph): Option[DsOliCell] = {
    val funSignature = classSignature + "getCell: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Calculate the linkage condition
    val sourceTargetCellsOpt = boxesCreator.minCond(e.lValueList.last.address, e.rValue.content, ptgI, boxesI)
    if (sourceTargetCellsOpt.isDefined) {
      val (source, _, offset) = sourceTargetCellsOpt.get
      // Calculate the vertex for the cell
      val vertex = ptgI.getMemoryVertex(ptgI.getVertexForAddress(source.bAddr).get)
      // Create a new EPT cell with the linkage offset and the cell offset within the vertex
      return Some(new DsOliEPTCell(source, offset, source.bAddr - vertex.bAddr))
    }
    None
  }

  /**
   * Fetch the most upstream cell before
   * the strands split, i.e., the last
   * cell while strands are still shared.
   * Note, that this method is intended for
   * the linear part of the strands only!
   *
   * @param box the strand
   * @param source the EPT cell
   * @param boxesI the strand set of the current time step (i)
   * @param ptgI the points-to graph of the current time step (i)
   * @return the EPT cell
   */
  def mostUpstreamCellBeforeSplit(box: DsOliBox, source: DsOliCell, boxesI: DsOliBoxStep, ptgI: DsOliGraph): DsOliCell = {
    val funSignature = classSignature + "mostUpstreamCellBeforeSplit: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Fetch the set of strands for the source cell
    val boxids = boxSet(source, boxesI)
    val boxOffset = box.offset

    // Iterate backwards through the linear part of 
    // the strand. Sharing can only be suspended
    // when moving upstream, thus checking for a 
    // change in the strand sets indicates that 
    // a strand has left the sharing, which means
    // using the previous position.
    var i = box.cells.indexOf(source) - 1
    while (i > 0 && boxids == boxSet(new DsOliEPTCell(box.cells(i), boxOffset, 0), boxesI)) {
      i -= 1
    }

    // Use the previous position of the cell, i.e., the last
    // cell which still had sharing
    val cell = box.cells(i + 1)
    val vertex = ptgI.getMemoryVertex(ptgI.getVertexForAddress(source.bAddr).get)
    val cellEPT = new DsOliEPTCell(cell, boxOffset, cell.bAddr - vertex.bAddr)
    DsOliLogger.debug(funSignature + "created ept cell: " + cellEPT)
    DsOliLogger.debug(funSignature + "\t used vertex: " + vertex)
    cellEPT
  }

  /**
   * Calculate the most upstream cell that will be used
   * by the surrogate EPT
   *
   * @param boxesI the strand set of the current time step (i)
   * @param source the source cell
   * @param ptgI the points-to graph of the current time step (i)
   * @return Option the EPT cell
   */
  def calculateCellForSurrogateEP(boxesI: DsOliBoxStep, source: DsOliCell, ptgI: DsOliGraph): Option[DsOliCell] = {
    val funSignature = classSignature + "calculateCellForSurrogateEP: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Non deterministic select one strand that contains source
    val boxTupleLinearOpt = boxesI.boxes.find(boxTuple => boxTuple._2.cells.contains(source)
      && boxTuple._2.offset == source.asInstanceOf[DsOliEPTCell].linkageOffset)

    var foundInCycle = false
    val boxTupleOpt = if (boxTupleLinearOpt.isEmpty) {
      boxesI.boxes.find {
        boxTuple =>
          val (boxId, box) = boxTuple

          // Check, if cyclic
          if (box.cycleId != 0 && boxesI.cycles.contains(box.cycleId)) {
            val cycle = boxesI.cycles.get(box.cycleId).get
            if (cycle.cells.exists(_ == source)) {
              foundInCycle = true
            }
            foundInCycle
          } else {
            false
          }

      }
    } else {
      boxTupleLinearOpt
    }
    if (boxTupleOpt.isEmpty) {
      return None
    } else {
      // Distinguish between cyclic and linear:
      // cyclic can be used immediately
      // linear search the most upstream 
      if (foundInCycle) {
        return Some(source)
      } else {
        return Some(mostUpstreamCellBeforeSplit(boxTupleOpt.get._2, source, boxesI, ptgI))
      }
    }

  }

  /**
   * Transfer surrogate EPTs to set of retired EPTs
   *
   * @param Fsur the set of surrogate EPTs
   * @param Fdone the set of retired EPTs
   * @param Fdel the set of EPTs to retire
   */
  def doBookkeepingOfEPTs(Fsur: DsOliFeatureSet, Fdone: DsOliFeatureSet, Fdel: ListBuffer[DsOliEPT]): Unit = {
    val funSignature = classSignature + "doBookkeepingOfEPTs: "
    DsOliLogger.debug(funSignature + "entered: ")
    Fdel.foreach {
      feature =>
        Fsur.features -= feature
        Fdone.features += feature
    }

  }

  /**
   * Remove all EPTs associated with the
   * free vertex.
   *
   * @param event the free/VLS event
   * @param Fsur the set of surrogate EPTs
   * @param Fdone the set of retired EPTs
   * @param ptgIminusOne the previous points-to graph
   */
  def cleanupFreeVLS(event: DsOliEvent, Fsur: DsOliFeatureSet, Fdone: DsOliFeatureSet, ptgIMinusOne: DsOliGraph): Unit = {
    val funSignature = classSignature + "cleanupFreeVLS: "
    DsOliLogger.debug(funSignature + "entered: ")

    // The freed address is stored differently in the
    // free and VLS event
    val addrOpt = condOpt(event) {
      case e: DsOliFreeEvent =>
        DsOliLogger.debug(funSignature + "found DsOliFreeEvent: " + e)
        e.argValue
      case e: DsOliVLSEvent =>
        DsOliLogger.debug(funSignature + "found DsOliVLSEvent: " + e)
        e.address
    }

    if (addrOpt.isDefined) {

      // Fetch the freed vertex
      val vertexEventOpt = ptgIMinusOne.getVertexForAddress(addrOpt.get)

      // Store the EPTs to delete
      val Fdel = new ListBuffer[DsOliEPT]()

      // Select all EPTs which were connected with the freed vertex
      Fsur.features.foreach {
        ept =>
          // Fetch vertex for the current EPT
          val vertexFeatureOpt = ptgIMinusOne.getVertexForAddress(ept.Aup.bAddr)

          // Compare the vertices if both are defined
          if (vertexFeatureOpt.isDefined && vertexEventOpt.isDefined) {
            if (vertexEventOpt.get == vertexFeatureOpt.get) {
              DsOliLogger.debug(funSignature + "found sept for deletion: " + ept)
              Fdel += ept
            }
          }

      }

      // Transfer surrogate EPTs to set of retired EPTs
      doBookkeepingOfEPTs(Fsur, Fdone, Fdel)
    }

  }

  /**
   * Checks, if the feature indicates
   * changes of the strand
   *
   * @param feature the feature to check
   * @return Boolean
   */
  def isNoChange(feature: Feature): Boolean = {

    // Explicitly list all events that changed
    // a strand
    feature match {
      // Changes
      case Feature.newlyCreated |
        Feature.plusOne |
        Feature.plusN |
        Feature.minusOne |
        Feature.minusN |
        Feature.freeOrVLS => false
      // Everything else => no change
      case _ => true
    }
  }

  /**
   * Surrogate EPTs get deleted after a configurable
   * amount of time steps, where they have not seen
   * any changes. This avoids clutter in the created
   * features and surrogate EPTs will be re-created
   * anyways if changes occur that require a
   * surrogate EPT.
   *
   * @param Fsur the set of surrogate EPTs
   * @param Fdone the set of retired EPTs
   */
  def aliveCheck(Fsur: DsOliFeatureSet, Fdone: DsOliFeatureSet): Unit = {
    val funSignature = classSignature + "aliveCheck: "
    DsOliLogger.debug(funSignature + "entered: ")
    val Fdel = new ListBuffer[DsOliEPT]()

    // Check all EPTs if they can be removed due
    // to no observed changes over the last n
    // time steps
    Fsur.features.foreach {
      ept =>
        DsOliLogger.debug(funSignature + "testing sept for deletion: " + ept)
        DsOliLogger.debug(funSignature + "\tept.Qf.length - eptTimeOut = " + (ept.Qf.length - eptTimeOut))

        // Only noChange events recorded for the last eptTimeOut steps
        if (ept.Qf.length >= eptTimeOut && ept.Qf.drop(ept.Qf.length - eptTimeOut).forall(isNoChange(_))) {
          DsOliLogger.debug(funSignature + "found sept for deletion: " + ept)
          Fdel += ept
        }

    }

    // Transfer surrogate EPTs to set of retired EPTs
    doBookkeepingOfEPTs(Fsur, Fdone, Fdel)
  }

  /**
   * Checks, if the surrogate EPT is actually
   * the most upstream. If the cell of the
   * memory write event is further upstream the
   * EPT can not be reused. This happens, if
   * the memory write event appends to the
   * beginning of a strand.
   *
   * @param Aup the cell of the surrogate EPT
   * @param curCell the current cell of the memory write event
   * @param boxesI the strand set of the current time step (i)
   * @param Boolean
   */
  def surEPTIsUpstream(Aup: DsOliEPTCell, curCell: DsOliEPTCell, boxesI: DsOliBoxStep): Boolean = {
    val funSignature = classSignature + "surEPTIsUpstream: "
    DsOliLogger.debug(funSignature + "entered")

    // Flags to indicate which cell
    // was found
    var foundAup = false
    var foundCurCell = false

    // Fetch strand which contains both Aup and curCell
    boxesI.boxes.foreach {
      boxTuple =>
        val (boxId, box) = boxTuple

        // Obey the linkage offset
        if (box.offset == curCell.linkageOffset) {
          DsOliLogger.debug(funSignature + "testing box: " + box)

          // Reset flags
          foundAup = false
          foundCurCell = false

          // Test the linear part of the strands
          box.cells.foreach {
            cell =>
              if (cell.id == Aup.id) {
                DsOliLogger.debug(funSignature + "Aup found")
                foundAup = true
              } else if (cell.id == curCell.id) {
                DsOliLogger.debug(funSignature + "curCell found")
                foundCurCell = true
              }
          }

          // Test if not both cells were found in the linear part
          if (!(foundAup && foundCurCell)) {

            // Duplicate flags to indicate what was found in 
            // linear part and what in cyclic part
            val alreadyFoundAup = foundAup
            val alreadyFoundCurCell = foundCurCell

            // Check cyclic part of strand
            if (box.cycleId != 0) {
              boxesI.cycles.get(box.cycleId).get.cells.foreach {
                cell =>
                  if (cell.id == Aup.id) {
                    DsOliLogger.debug(funSignature + "Aup found")
                    foundAup = true
                  } else if (cell.id == curCell.id) {
                    DsOliLogger.debug(funSignature + "curCell found")
                    foundCurCell = true
                  }
              }
            }

            // Need to distinguish the different combinations of 
            // linear and cyclic parts to decide if cells are
            // more upstream

            // Aup is in linear part, cur cell is in cycle
            if (alreadyFoundAup && foundCurCell) {
              DsOliLogger.debug(funSignature + "Aup is in linear part, cur cell is in cycle: true")
              return true
              // Current cell is in linear part, Aup is in cycle
            } else if (alreadyFoundCurCell && foundAup) {
              DsOliLogger.debug(funSignature + "Current cell is in linear part, Aup is in cycle: false")
              return false
              // Both are in cycle
            } else if (foundAup && foundCurCell) {
              DsOliLogger.debug(funSignature + "Aup and cur cell is in cycle: true")
              return true
              // One cell was not found
            } else {
              DsOliLogger.debug(funSignature + "Found none in cycle: false")
              return false
            }

          } else {
            // Both in linear part
            // Test if Aup is further upstream than curCell
            if (foundAup && foundCurCell) {
              DsOliLogger.debug(funSignature + "indexOf Aup: " + box.cells.indexOf(Aup))
              DsOliLogger.debug(funSignature + "indexOf curCell: " + box.cells.indexOf(curCell))

              // Lower index means further upstream
              return box.cells.indexOf(Aup) < box.cells.indexOf(curCell)
            } else {
              DsOliLogger.debug(funSignature + "did not find both cells: " + box)
            }

          }
        } else {
          DsOliLogger.debug(funSignature + "skipping box: " + box)
        }
    }
    DsOliLogger.debug(funSignature + "Found nothing: false")
    false
  }

  /**
   * Insert surrogate EPTs if an event occurred that
   * actually changed the strand sets and no real
   * EPT observed the changes.
   *
   * @param event the event which triggered the changes
   * @param boxesIMinusOne the strand set at the previous time step (i-1)
   * @param boxesI the strand set of the current time step (i)
   * @param Fsur the feature set of live surrogate EPTs
   * @param Fbuilding the feature set of live EPTs
   * @param ptgI the points-to graph of the current time step (i)
   * @param i the time step
   */
  def insertSurrogateEPIfNeeded(event: DsOliEvent, boxesIMinusOne: DsOliBoxStep, boxesI: DsOliBoxStep,
    Fsur: DsOliFeatureSet, Fbuilding: DsOliFeatureSet, ptgI: DsOliGraph, i: Long): Unit = {
    val funSignature = classSignature + "insertSurrogateEPIfNeeded: "
    DsOliLogger.debug(funSignature + "entered: Fsur.length = " + Fsur.features.length + " Fbuilding.length = " + Fbuilding.features.length)

    // Consider memory write events
    if (event.isInstanceOf[DsOliMWEvent]) {

      DsOliLogger.debug(funSignature + "DSOliMWEvent")
      DsOliLogger.debug(funSignature + "boxIMinusOne.keySet: " + boxesIMinusOne.boxes.keySet)
      DsOliLogger.debug(funSignature + "boxI.keySet: " + boxesI.boxes.keySet)

      // Difference of box sets between time steps: returns changed strands
      val boxidDiff = boxesIMinusOne.boxes.keySet.diff(boxesI.boxes.keySet)

      DsOliLogger.debug(funSignature + "calculate boxSetsChanged")
      // Box sets have changed?
      val boxSetsChanged = (boxesIMinusOne.boxes.keySet != boxesI.boxes.keySet)

      DsOliLogger.debug(funSignature + "calculate noRealEPTCapture")
      // No real EPT has captured the event? No EPT captured a change
      val noRealEPTCapture = !Fbuilding.features.exists { ept =>
        DsOliLogger.debug(funSignature + "noRealEPTCapture: feature = " + ept.Qf.last)
        !isNoChange(ept.Qf.last)
      }

      DsOliLogger.debug(funSignature + "calculate noSurEPTCapture")
      // No existing surrogate EPT can capture the event?
      val noSurEPTCapture = !Fsur.features.exists {
        ept =>
          DsOliLogger.debug(funSignature + "testing ept: " + ept)
          val boxIds = boxSet(ept.Aup, boxesIMinusOne)

          // Compute the intersection between the changed strands 
          // and the strands seen by the current EPT. If there 
          // is an intersection, the EPT can observe the changes
          val oldEPT = boxidDiff.&(boxIds).size != 0

          // Debug
          DsOliLogger.debug(funSignature + "boxIds: " + boxIds)
          DsOliLogger.debug(funSignature + "boxidDiff: " + boxidDiff)
          DsOliLogger.debug(funSignature + "boxidDiff intersection boxIds: " + boxidDiff.&(boxIds))
          DsOliLogger.debug(funSignature + "(boxidDiff intersection boxIds).size: " + boxidDiff.&(boxIds).size)
          DsOliLogger.debug(funSignature + "oldEPT: " + oldEPT)

          // Get the EPT cell for the event
          val curCellOpt = getCell(event.asInstanceOf[DsOliMWEvent], boxesI, ptgI)

          // The old EPT can record the changes and the current EPT cell is defined
          // and the surrogate EPT is the most upstream EPT
          oldEPT && (curCellOpt.isDefined && surEPTIsUpstream(ept.Aup, curCellOpt.get.asInstanceOf[DsOliEPTCell], boxesI))
      }

      // The strands changed and no real EPT and no surrogate EPT captured the event? => create new surrogate EPT
      if (boxSetsChanged && noRealEPTCapture && noSurEPTCapture) {

        DsOliLogger.debug(funSignature + "Need new ept")

        // Calculate the surrogate EPT
        val sourceOpt = getCell(event.asInstanceOf[DsOliMWEvent], boxesI, ptgI)
        if (sourceOpt.isDefined) {
          DsOliLogger.debug(funSignature + "Creating new ept for source cell: " + sourceOpt.get)

          // Calculate the most upstream cell for the surrogate EPT
          val Aup = calculateCellForSurrogateEP(boxesI, sourceOpt.get, ptgI)
          if (Aup.isDefined) {
            DsOliLogger.debug(funSignature + "found Aup: " + Aup)
            // New ept 
            val newEpt = new DsOliEPT(new DsOliVertexMemory(0, 0, new DsOliType(null, "sep"), 0), 0, 0,
              Aup.get.asInstanceOf[DsOliEPTCell], i, new ListBuffer[Feature]())
            newEpt.Qf.append(Feature.newlyCreated)
            Fsur.features.append(newEpt)
          }

        }
      } else {
        DsOliLogger.debug(funSignature + "boxSetsChanged && noRealEPTCapture && noSurEPTCapture != true: " +
          boxSetsChanged + ", " + noRealEPTCapture + ", " + noSurEPTCapture)
      }

    } else {
      DsOliLogger.debug(funSignature + "event not in specified surrogate events: " + event)
    }
  }

  /**
   * Handle surrogate EPTs: insert, update, cleanup
   * Surrogate EPTs are used to capture changes of
   * the strands which would not be visible with
   * only the EPTs.
   *
   * @param event the event which triggered the changes
   * @param boxesIMinusOne the strand set at the previous time step (i-1)
   * @param boxesI the strand set of the current time step (i)
   * @param Fsur the feature set of live surrogate EPTs
   * @param Fdone the feature set of retired EPTs
   * @param Fbuilding the feature set of live EPTs
   * @param ptgIMinusOne the points-to graph of the previous time step (i-1)
   * @param ptgI the points-to graph of the current time step (i)
   * @param i the time step
   */
  def updateSurrogateEPs(event: DsOliEvent, boxesIMinusOne: DsOliBoxStep, boxesI: DsOliBoxStep,
    Fsur: DsOliFeatureSet, Fdone: DsOliFeatureSet, Fbuilding: DsOliFeatureSet, ptgIMinusOne: DsOliGraph, ptgI: DsOliGraph, i: Long): Unit = {
    val funSignature = classSignature + "updateSurrogateEPs: "
    DsOliLogger.debug(funSignature + "entered")

    // Cleanup in case of free|vls
    cleanupFreeVLS(event, Fsur, Fdone, ptgIMinusOne)

    // Alive check 
    aliveCheck(Fsur, Fdone)

    // Insert surrogate ept
    insertSurrogateEPIfNeeded(event, boxesIMinusOne, boxesI, Fsur, Fbuilding, ptgI, i)

    // Update all surrogate epts
    updateAllOtherEPTs(boxesIMinusOne, boxesI, ptgI, Fsur, i.toInt)

    DsOliLogger.debug(funSignature + "updateUnchangedEPTs will be called")

    // Update all unchanged surrogate epts
    updateUnchangedEPTs(i.toInt, event, Fsur)
  }

  /**
   * Calculate the changes to the EPTs due
   * to a pointer write.
   *
   * @param e the memory write event
   * @param i the time step
   * @param boxesIMinusOne the strand set at the previous time step (i-1)
   * @param boxesI the strand set of the current time step (i)
   * @param ptgIMinusOne the points-to graph of the previous time step (i-1)
   * @param ptgI the points-to graph of the current time step (i)
   * @param Fdone the feature set of retired EPTs
   * @param Fbuilding the feature set of live EPTs
   * @param Fsur the feature set of live surrogate EPTs
   */
  def calculateEPTsForPointerWrite(e: DsOliMWEvent, i: Int, boxesIMinusOne: DsOliBoxStep, boxesI: DsOliBoxStep, ptgIMinusOne: DsOliGraph, ptgI: DsOliGraph,
    Fdone: DsOliFeatureSet, Fbuilding: DsOliFeatureSet, Fsur: DsOliFeatureSet): Unit = {
    val funSignature = classSignature + "calculateEPTsForPointerWrite: "
    DsOliLogger.debug(funSignature + " memory write event")

    // Get the old target address
    val targetAddressOldOpt = ptgIMinusOne.getCurrentTargetAddr(e.sourceAddress + e.sourceOffset)

    // If old address was not found we got a new one. Otherwise we need to check, if we assign to the previous address
    if (targetAddressOldOpt.isEmpty || e.rValue.content != targetAddressOldOpt.get) {

      DsOliLogger.debug(funSignature + "newly created target || targetAddress != targetAddressOld")
      DsOliLogger.debug(funSignature + "testing if isEP can be called on old addresses")

      // First check if the old edge was an EP
      val epOldOpt = if (targetAddressOldOpt.isEmpty) None else isEP(e.lValueList.last.address, targetAddressOldOpt.get, boxesIMinusOne, ptgIMinusOne)

      DsOliLogger.debug(funSignature + "calling isEP on new addresses")
      // Check if the new edge is an EP
      val epOpt = isEP(e.sourceAddress + e.sourceOffset, e.rValue.content, boxesI, ptgI)

      // Was entry pointer before, is NONE now
      if (epOldOpt.isDefined && epOpt.isEmpty) {
        retireEPTsForCells(epOldOpt.get, Fdone, Fbuilding)
      }

      // Is entry pointer now (previous role of ep is irrelevant here)
      if (epOpt.isDefined) {
        val targetVertex = ptgI.getVertexForAddress(e.rValue.content)
        calculateEPWriteChanges(ptgI.getMemoryVertex(targetVertex.get), epOpt.get, boxesIMinusOne, boxesI, i, Fbuilding, Fdone)
      }

      // Fulfilled a minimum condition before
      val sourceTargetCellsOldOpt = if (targetAddressOldOpt.isEmpty) {
        None
      } else {
        boxesCreator.minCond(e.lValueList.last.address, targetAddressOldOpt.get, ptgIMinusOne, boxesIMinusOne)
      }
      if (sourceTargetCellsOldOpt.isDefined) {
        DsOliLogger.debug(funSignature + "minimum condition fulfilled on old link")
        val (sourceCell, targetCell, offset) = sourceTargetCellsOldOpt.get

        // Update the EPTs for the source cell
        updateEPTsForCellGroups(sourceCell, boxesIMinusOne, boxesI, ptgI, i, Fdone, Fbuilding)

        // Update the EPTs for the target cell, if they are
        // not within the same vertex
        val sourceVertex = ptgIMinusOne.getMemoryVertex(ptgIMinusOne.getVertexForAddress(sourceCell.bAddr).get)
        val targetVertex = ptgIMinusOne.getMemoryVertex(ptgIMinusOne.getVertexForAddress(targetCell.bAddr).get)
        if (sourceVertex.id != targetVertex.id) {
          updateEPTsForCellGroups(targetCell, boxesIMinusOne, boxesI, ptgI, i, Fdone, Fbuilding)
        }
      }

      // Fulfilled a minimum condition after
      val sourceTargetCellsOpt = boxesCreator.minCond(e.lValueList.last.address, e.rValue.content, ptgI, boxesI)
      if (sourceTargetCellsOpt.isDefined) {
        DsOliLogger.debug(funSignature + "minimum condition fulfilled on new link")
        val (sourceCell, targetCell, offset) = sourceTargetCellsOpt.get
        if (sourceTargetCellsOldOpt.isEmpty) {
          DsOliLogger.debug(funSignature + "running on S")
          updateEPTsForCellGroups(sourceCell, boxesIMinusOne, boxesI, ptgI, i, Fdone, Fbuilding)
        }
        if (ptgI.getVertexForAddress(sourceCell.bAddr) != ptgI.getVertexForAddress(targetCell.bAddr)) {
          DsOliLogger.debug(funSignature + "running on T")
          updateEPTsForCellGroups(targetCell, boxesIMinusOne, boxesI, ptgI, i, Fdone, Fbuilding)
        }
      }

      // Update all EPTs which where not changed yet
      updateAllOtherEPTs(boxesIMinusOne, boxesI, ptgI, Fbuilding, i)

    } else {
      DsOliLogger.debug(funSignature + " e.targetAddress == targetAddressOld: " + e.rValue.content + " == " + targetAddressOldOpt.get)
    }

  }

  /**
   * Move the EPT from one set to the other
   * and keep all recorded features. Mainly
   * used for transferring EPTs from the
   * artificial set to the real set of EPTs.
   *
   * @param i the current time step
   * @param ept the EPT to move
   * @param from the EPT set to remove the given EPT from
   * @param to the EPT set to move the given EPT to
   * @return Boolean indicating if the EPT could be moved
   */
  def moveEPT(i: Int, ept: DsOliEPT, from: DsOliFeatureSet, to: DsOliFeatureSet): Boolean = {
    val funSignature = classSignature + "moveEPT: "

    // Does the EPT exist in the 'from' set
    val movedEPTOpt = from.features.find(testEPT => testEPT.id == ept.id)

    if (movedEPTOpt.isDefined) {
      DsOliLogger.warning(funSignature + "moving ept from -> to : " + ept)
      val movedEPT = movedEPTOpt.get

      // Store the artificial features by removing the
      // trailing features and storing the remaining
      // into the artificial feature sequence
      val dropItems = (i - ept.creationTime).toInt
      val artFeatures = ept.Qf.drop(dropItems)
      movedEPT.artificialQfs.put(i, artFeatures)

      // Move EPT from -> to
      to.features.append(movedEPT)
      from.features -= movedEPT

      true
    } else {
      DsOliLogger.warning(funSignature + "did not find ept in from: " + ept)
      false
    }
  }

  /**
   * Handling of all artificial (real and surrogate) EPTs which
   * are still life. The artificial features are stored in the
   * dedicated artificial feature sequence, and the real features
   * are updated according to the event that triggered  the
   * artificial changes.
   *
   * @param i the current time step
   * @param F the real EPT set to move the artificial events to
   * @param Frt the artificial EPT set to move to the real EPT set
   */
  def transferArtificialEPTs(i: Int, F: DsOliFeatureSet, Frt: DsOliFeatureSet, event: DsOliEvent): Unit = {
    val funSignature = classSignature + "transferArtificialEPTs: "

    // Save all surrogate EPTs which were created during the artificial events
    Frt.features.foreach {
      ept =>
        // New EPT was created
        if (!F.features.exists(testEPT => testEPT.id == ept.id)) {
          // Store the artificial feature trace for this time step
          ept.artificialQfs.put(i, ept.Qf)

          // Now create a new feature trace which only shows, that this
          // EPT was created during the event
          ept.Qf = new ListBuffer[Feature]()
          ept.Qf += newlyCreated
          F.features.append(ept)

        } else {
          // Here the EPT was already present before, so it was developed further
          val existingEPTOpt = F.features.find(testEPT => testEPT.id == ept.id)
          if (existingEPTOpt.isDefined) {

            // Extract the artificial features
            val existingEPT = existingEPTOpt.get
            val dropItems = (i - ept.creationTime).toInt
            val artFeatures = ept.Qf.drop(dropItems)
            // Store the artificial feature traces for this EPT
            existingEPT.artificialQfs.put(i, artFeatures)

            // Debug
            DsOliLogger.debug(funSignature + "dropping items: " + dropItems)
            DsOliLogger.debug(funSignature + "existing ept(" + existingEPT.Qf.length + "): " + existingEPT.Qf)
            DsOliLogger.debug(funSignature + "art before drop(" + ept.Qf.length + "): " + ept.Qf)
            DsOliLogger.debug(funSignature + "art after(" + artFeatures.length + "): " + artFeatures)

            // Now update the real EPT feature
            // If we have a free/vls event and not all the recorded artificial features are no change -> record a free/vls
            if (DsOliEventUtils.isFreeOrVLS(event) && !artFeatures.forall(isNoChange(_))) {
              existingEPT.Qf += freeOrVLS
              // All artificials have seen a no change event -> record noChange
            } else if (artFeatures.forall(isNoChange(_))) {
              existingEPT.Qf += noChange
              // Removal of multiple elements at once is recorded
            } else if (artFeatures.exists(_ == minusN)) {
              existingEPT.Qf += minusN
              // Removal of one element is recorded
            } else if (artFeatures.count(_ == minusOne) == 1) {
              existingEPT.Qf += minusOne
              // Removal of multiple elements during the artificial phase is recorded as multi remove
            } else if (artFeatures.count(_ == minusOne) > 1) {
              existingEPT.Qf += minusN
              // Else we do not know what to do: noChange
            } else {
              existingEPT.Qf += noChange
            }
          }
        }
    }

  }

  /**
   * Keep track of the artificial changes of the EPTs
   *
   * @param event the event that caused the artificial changes
   * @param i the time step
   * @param FdoneArt the feature set of retired artificial EPTs
   * @param FbuildingArt the feature set of live artificial EPTs
   * @param FsurArt the feature set of live surrogate artificial EPTs
   * @param Fsur the feature set of live surrogate EPTs
   * @param Fdone the feature set of retired EPTs
   * @param Fbuilding the feature set of live EPTs
   */
  def calculateArtificialChanges(event: DsOliEvent, i: Int, FdoneArt: DsOliFeatureSet, FbuildingArt: DsOliFeatureSet, FsurArt: DsOliFeatureSet,
    Fdone: DsOliFeatureSet, Fbuilding: DsOliFeatureSet, Fsur: DsOliFeatureSet): Unit = {
    val funSignature = classSignature + "calculateArtificialChanges: "
    DsOliLogger.debug(funSignature + "entered")

    DsOliLogger.debug(funSignature + "Fdone.features.length = " + Fdone.features.length)
    DsOliLogger.debug(funSignature + "FdoneArt.features.length = " + FdoneArt.features.length)

    // Retire all EPTs which were retired during artificial events
    FdoneArt.features.foreach {
      ept =>
        DsOliLogger.warning(funSignature + "processing artificial ept: " + ept)
        // Find all the newly retired EPTs. Important: the check for ept.creationTime < i as this will filter out 
        // EPTs which were created during the artificial events AND retired during this phase. We are not interested
        // in those EPTs!
        if (ept.creationTime < i) {
          // The EPT in the artificial done set can stem from real or surrogate set
          if (!moveEPT(i, ept, Fbuilding, Fdone) && !moveEPT(i, ept, Fsur, Fdone)) {
            DsOliLogger.warning(funSignature + "unable to move ept (must be newly created during artificial events): " + ept)
          } else {
            DsOliLogger.warning(funSignature + "Moving ept: " + ept)
          }
        } else {
          DsOliLogger.warning(funSignature + "Skipping ept for retirement: " + ept)
        }
    }

    DsOliLogger.debug(funSignature + "transfering surrogate pointers.")
    // Transfer all surrogate EPTs which were touched during the artificial events
    transferArtificialEPTs(i, Fsur, FsurArt, event)

    DsOliLogger.debug(funSignature + "transfering real pointers .")
    // Transfer all real EPTs which were touched during the artificial events
    transferArtificialEPTs(i, Fbuilding, FbuildingArt, event)
  }

  /**
   * Delete all EPTs which are no longer connected
   * to any strands.
   *
   * @param F the life EPT set to check
   * @param Fdone the retired EPT set
   * @param boxesI the strand set of the current time step (i)
   */
  def deleteEPTs(F: DsOliFeatureSet, Fdone: DsOliFeatureSet, boxesI: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "deleteEPTs: "
    DsOliLogger.debug(funSignature + "entered: ")

    // List of EPTs to delete
    val delF = new ListBuffer[DsOliEPT]()

    // Check through all EPTs and retire EPTs which
    // are not longer connected to any strand in the
    // the current strand set.
    F.features.foreach {
      ept =>

        DsOliLogger.debug(funSignature + "testing ept: " + ept)

        // Fetch the strands for the EPT
        val boxIdSet = boxSet(ept.Aup, boxesI)

        DsOliLogger.debug(funSignature + "boxIdSet: " + boxIdSet)
        DsOliLogger.debug(funSignature + "boxesI.boxes.keySet: " + boxesI.boxes.keySet)

        // If no strands retire EPT
        if (boxIdSet.isEmpty) {
          DsOliLogger.debug(funSignature + "found ept to delete: " + ept)
          // Add into set of retired EPTs
          Fdone.features += ept
          // Mark for delete from life EPT set
          delF.append(ept)
        }
    }

    DsOliLogger.debug(funSignature + "deleting epts: " + delF)
    // Actually do the deletion of the EPTs 
    // from the life tset
    delF.foreach {
      F.features -= _
    }
  }

  /**
   * Remove the EPTs from both the life set
   * of strands and the life surrogate EPTs.
   *
   * @param event the event to check
   * @param Fdone the set of retired EPTs
   * @param Fubilding the set of life EPTs
   * @param Fsur the set of life surrogate EPTs
   * @param boxesI the strand set of the current time step (i)
   */
  def removeEPTsViaBoxes(event: DsOliEvent, Fdone: DsOliFeatureSet, Fbuilding: DsOliFeatureSet, Fsur: DsOliFeatureSet,
    boxesI: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "removeEPTsViaBoxes: "
    DsOliLogger.debug(funSignature + "entered: " + event)
    deleteEPTs(Fbuilding, Fdone, boxesI)
    deleteEPTs(Fsur, Fdone, boxesI)
  }

  def removeEPTs(event: DsOliEvent, Fdone: DsOliFeatureSet, Fbuilding: DsOliFeatureSet, Fsur: DsOliFeatureSet,
    ptgIMinusOne: DsOliGraph, boxesIMinusOne: DsOliBoxStep, boxesI: DsOliBoxStep): Unit = {
    val funSignature = classSignature + "removeEPTs: "
    DsOliLogger.debug(funSignature + "entered: " + event)

    val addressOpt = DsOliAddressUtils.getStartAddressFromFreeVLS(event)
    if (addressOpt.isDefined) {
      DsOliLogger.debug(funSignature + "is free/vls.")
      val removedVertexOpt = ptgIMinusOne.getVertexForAddress(addressOpt.get)

      if (removedVertexOpt.isDefined) {
        val removedVertex = ptgIMinusOne.getMemoryVertex(removedVertexOpt.get)
        DsOliLogger.debug(funSignature + "found removed vertex: " + removedVertex)

        val delF = new ListBuffer[DsOliEPT]()
        Fbuilding.features.foreach {
          ept =>
            if (ept.epVertexId == removedVertex.id) {
              DsOliLogger.debug(funSignature + "moving ept to Fdone: " + ept)
              Fdone.features += ept
              delF.append(ept)
            }
        }

        DsOliLogger.debug(funSignature + "deleting epts: " + delF)
        delF.foreach {
          Fbuilding.features -= _
        }
      }
    } else {
      // Record error
      DsOliLogger.error(funSignature + "not called  with free/vls event: " + event)
    }

  }

  /**
   * Process an event, that has NO artificial events
   *
   * @param event the event with artificial events
   * @param i the current time step
   * @param Fdone the set of retired EPTs
   * @param Fbuilding the set of life EPTs
   * @param Fsur the set of surrogate EPTs
   */
  def processEvent(event: DsOliEvent, i: Int,
    Fdone: DsOliFeatureSet, Fbuilding: DsOliFeatureSet, Fsur: DsOliFeatureSet) = {
    val funSignature = classSignature + "processEvent: "
    // Event without artificial events

    //  Fetch the previous and strand set and PTG
    val boxesIMinusOneOpt = boxes.get(i - 1)
    if (boxesIMinusOneOpt.isEmpty) {
      throw new Exception(funSignature + " boxesIMinusOneOpt.isEmpty = " + boxesIMinusOneOpt.isEmpty)
    }
    var boxesIMinusOne = boxesIMinusOneOpt.get
    var ptgIMinusOne = ptgs.get(i - 1)

    // Fetch the current PTG and strand set
    val ptgI = ptgs.get(i)
    val boxesI = boxes.get(i).get

    // Check, which event we got
    event match {
      case e: DsOliMWEvent =>
        calculateEPTsForPointerWrite(e, i, boxesIMinusOne, boxesI, ptgIMinusOne, ptgI, Fdone, Fbuilding, Fsur)
      case e: DsOliVLSEvent =>
        removeEPTsViaBoxes(e, Fdone, Fbuilding, Fsur, boxesI)
      case e: DsOliFreeEvent =>
        // Most unlikely, that this occurs without artificial events!
        removeEPTsViaBoxes(e, Fdone, Fbuilding, Fsur, boxesI)
      case _ => // No other events
    }

    updateUnchangedEPTs(i, event, Fbuilding)
    updateSurrogateEPs(event, boxesIMinusOne, boxesI, Fsur, Fdone, Fbuilding, ptgIMinusOne, ptgI, i)

  }

  /**
   * Process an event, that has artificial events
   *
   * @param event the event with artificial events
   * @param i the current time step
   * @param Fdone the set of retired EPTs
   * @param Fbuilding the set of life EPTs
   * @param Fsur the set of surrogate EPTs
   */
  def processArtificialEvents(event: DsOliEvent, i: Int,
    Fdone: DsOliFeatureSet, Fbuilding: DsOliFeatureSet, Fsur: DsOliFeatureSet) = {
    val funSignature = classSignature + "processArtificialEvents: "

    //  Fetch the previous and strand set and PTG
    val boxesIMinusOneOpt = boxes.get(i - 1)
    if (boxesIMinusOneOpt.isEmpty) {
      throw new Exception(funSignature + " boxesIMinusOneOpt.isEmpty = " + boxesIMinusOneOpt.isEmpty)
    }
    var boxesIMinusOne = boxesIMinusOneOpt.get
    var ptgIMinusOne = ptgs.get(i - 1)

    // The artificial event has the same sets
    // for recording EPTs and features
    val FdoneArt = new DsOliFeatureSet()
    val FbuildingArt = Fbuilding.deepCopy
    val FsurArt = Fsur.deepCopy

    DsOliLogger.debug(funSignature + "event has artificial events: " + event)
    DsOliLogger.debug(funSignature + "\tevents(" + event.artificialEvents.length + "): " + event.artificialEvents)

    // The time steps for artificial events
    var u = 0

    // Process all intermediate artificial events
    event.artificialEvents.foreach {
      artificialEvent =>

        DsOliLogger.debug("\t" + funSignature + "artificial event " + event.id + ":" + u + ": " + artificialEvent)

        // Fetch the current PTGs and strand set
        val ptgI = ptgs.get(i).artificialGraphs(u)
        val boxesI = boxes.get(i).get.artificialBoxes(u)

        // Now check which artificial event we got
        artificialEvent match {
          case e: DsOliArtificialUndefEvent =>
            DsOliLogger.debug("\t" + funSignature + "artificial undef event: " + e)
            calculateEPTsForPointerWrite(e, i + u, boxesIMinusOne, boxesI, ptgIMinusOne, ptgI, FdoneArt, FbuildingArt, FsurArt)

          case e: DsOliMWEvent =>
            DsOliLogger.debug("\t" + funSignature + "original artificial memory write event: " + e)
            // Important: i + u keeps the artificial clock ticking!
            calculateEPTsForPointerWrite(e, i + u, boxesIMinusOne, boxesI, ptgIMinusOne, ptgI, FdoneArt, FbuildingArt, FsurArt)

          case e: DsOliArtificialFreeEvent =>
            DsOliLogger.debug("\t" + funSignature + "artificial free event: " + e)
            removeEPTsViaBoxes(e, FdoneArt, FbuildingArt, FsurArt, boxesI)

          case e: DsOliFreeEvent =>
            DsOliLogger.debug("\t" + funSignature + "original artificial free event: " + e)
            removeEPTsViaBoxes(e, FdoneArt, FbuildingArt, FsurArt, boxesI)

          case e: DsOliVLSEvent =>
            DsOliLogger.debug("\t" + funSignature + "original artificial vls event: " + e)
            removeEPTsViaBoxes(e, FdoneArt, FbuildingArt, FsurArt, boxesI)

          case _ => throw new Exception(funSignature + "unknown event: " + artificialEvent)
        }

        updateUnchangedEPTs(i + u, artificialEvent, FbuildingArt)
        updateSurrogateEPs(artificialEvent, boxesIMinusOne, boxesI, FsurArt, FdoneArt, FbuildingArt, ptgIMinusOne, ptgI, i + u)

        // The current PTG and strand set become the
        // previous ones
        ptgIMinusOne = ptgI
        boxesIMinusOne = boxesI

        // Tick the artificial event clock
        u += 1
    }

    calculateArtificialChanges(event, i, FdoneArt, FbuildingArt, FsurArt, Fdone, Fbuilding, Fsur)

  }

  /**
   * Create entry pointer tags (EPT) to observe
   * changes in a data structure, called features.
   * The algorithm also records the actual changes
   * in the data structures (e.g., adding/removing
   * elements from a strand) and artificially
   * creates surrogate entry pointers if
   * required.
   *
   * @return the recorded EPTs and their associated features
   */
  def createEPTs(): DsOliFeatureSet = {
    val funSignature = classSignature + "createEPTs:"

    // The set of EPTs which are retired
    val Fdone = new DsOliFeatureSet()
    // Fbuilding is now called Freal in pseudo code
    // The set of currently active EPTs
    val Fbuilding = new DsOliFeatureSet()
    // The set of surrogate EPTs
    val Fsur = new DsOliFeatureSet()

    var i = 0

    // Process each event
    this.events.events.foreach {
      event =>
        i += 1

        DsOliLogger.debug(funSignature + "#Event " + event.id + "# Step " + i + " entry point event " + event.id + "  ****")
        DsOliLogger.debug(funSignature + "\tevent: " + event)
        print("EPT: " + i + "/" + this.events.events.size + "\r")

        // Distinguish between an event with artificial events
        // and one without artificial events
        if (event.artificialEvents.length != 0) {
          processArtificialEvents(event, i, Fdone, Fbuilding, Fsur)
        } else {
          processEvent(event, i, Fdone, Fbuilding, Fsur)
        }

        DsOliLogger.debug(funSignature + "#Done Event " + event.id + "# Step " + i + " done entry point event " + event.id + "  ****")

    }

    // Finally retire all remaining EPTs
    Fdone.features.appendAll(Fbuilding.features)
    Fdone.features.appendAll(Fsur.features)

    return Fdone
  }
}