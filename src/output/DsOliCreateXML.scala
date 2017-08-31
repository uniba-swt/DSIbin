
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
 * DsOliCreateXML.scala created on Dec 11, 2015
 *
 * Description: Create the XML output to be used by various backends
 */
package output

import pointstograph.DsOliPointsToGraphs
import boxcalculation.DsOliBoxSteps
import pointstograph.ITypeDB
import scala.collection.mutable.HashMap
import pointstograph.DsOliVertex
import pointstograph.DsOliVertexMemory
import pointstograph.DsOliDiEdge
import boxcalculation.DsOliCell
import boxcalculation.DsOliBox
import scala.collection.mutable.ListBuffer
import boxcalculation.DsOliBoxesCreator
import java.io.PrintWriter
import java.io.File
import test.DsOliTestMethods
import util.DsOliPathUtils

/**
 * @author DSI
 *
 */
class DsOliCreateXML(ptgs: DsOliPointsToGraphs, boxSteps: DsOliBoxSteps, typeDB: ITypeDB, boxesCreator: DsOliBoxesCreator) {

  val paddingIndent = "    "

  val position = "position"
  val positionOpen = "<" + position + ">"
  val positionClose = "</" + position + ">"

  val ptg = "ptg"
  val ptgOpen = "<" + ptg + ">"
  val ptgClose = "</" + ptg + ">"

  val addr = "addr"
  val addrOpen = "<" + addr + ">"
  val addrClose = "</" + addr + ">"

  val addrAsVertex = "addrAsVertex"
  val addrAsVertexOpen = "<" + addrAsVertex + ">"
  val addrAsVertexClose = "</" + addrAsVertex + ">"

  val bAddr = "bAddr"
  val bAddrOpen = "<" + bAddr + ">"
  val bAddrClose = "</" + bAddr + ">"

  val eAddr = "eAddr"
  val eAddrOpen = "<" + eAddr + ">"
  val eAddrClose = "</" + eAddr + ">"

  val strand = "strand"
  val strandOpen = "<" + strand + ">"
  val strandClose = "</" + strand + ">"

  val strands = "strands"
  val strandsOpen = "<" + strands + ">"
  val strandsClose = "</" + strands + ">"

  val strandId = "id"
  val strandIdOpen = "<" + strandId + ">"
  val strandIdClose = "</" + strandId + ">"

  val cell = "cell"
  val cellOpen = "<" + cell + ">"
  val cellClose = "</" + cell + ">"

  val cellType = "cellType"
  val cellTypeOpen = "<" + cellType + ">"
  val cellTypeClose = "</" + cellType + ">"

  val cellRef = "cellRef"
  val cellRefOpen = "<" + cellRef + ">"
  val cellRefClose = "</" + cellRef + ">"

  val cellPosition = "position"
  val cellPositionOpen = "<" + cellPosition + ">"
  val cellPositionClose = "</" + cellPosition + ">"

  val cellId = "cellId"
  val cellIdOpen = "<" + cellId + ">"
  val cellIdClose = "</" + cellId + ">"

  val cells = "cells"
  val cellsOpen = "<" + cells + ">"
  val cellsClose = "</" + cells + ">"

  val CpointerHex = "CpointerHex"
  val CpointerHexOpen = "<" + CpointerHex + ">"
  val CpointerHexClose = "</" + CpointerHex + ">"

  val cellSequences = "cellSequences"
  val cellSequencesOpen = "<" + cellSequences + ">"
  val cellSequencesClose = "</" + cellSequences + ">"

  val cellSequence = "cellSequence"
  val cellSequenceOpen = "<" + cellSequence + ">"
  val cellSequenceClose = "</" + cellSequence + ">"

  val cyclicSequence = "cyclicSequence"
  val cyclicSequenceOpen = "<" + cyclicSequence + ">"
  val cyclicSequenceClose = "</" + cyclicSequence + ">"

  val linearSequence = "linearSequence"
  val linearSequenceOpen = "<" + linearSequence + ">"
  val linearSequenceClose = "</" + linearSequence + ">"

  val cycle = "cycle"
  val cycleOpen = "<" + cycle + ">"
  val cycleClose = "</" + cycle + ">"

  val cycleId = "cycleId"
  val cycleIdOpen = "<" + cycleId + ">"
  val cycleIdClose = "</" + cycleId + ">"

  val cycles = "cycles"
  val cyclesOpen = "<" + cycles + ">"
  val cyclesClose = "</" + cycles + ">"

  val edge = "edge"
  val edgeOpen = "<" + edge + ">"
  val edgeClose = "</" + edge + ">"

  val edgeId = "id"
  val edgeIdOpen = "<" + edgeId + ">"
  val edgeIdClose = "</" + edgeId + ">"

  val edges = "edges"
  val edgesOpen = "<" + edges + ">"
  val edgesClose = "</" + edges + ">"

  val id = "id"
  val idOpen = "<" + id + ">"
  val idClose = "</" + id + ">"

  val linkageOffset = "linkageOffset"
  val linkageOffsetOpen = "<" + linkageOffset + ">"
  val linkageOffsetClose = "</" + linkageOffset + ">"

  val name = "name"
  val nameOpen = "<" + name + ">"
  val nameClose = "</" + name + ">"

  val offset = "offset"
  val offsetOpen = "<" + offset + ">"
  val offsetClose = "</" + offset + ">"

  val size = "size"
  val sizeOpen = "<" + size + ">"
  val sizeClose = "</" + size + ">"

  val source = "source"
  val sourceOpen = "<" + source + ">"
  val sourceClose = "</" + source + ">"

  val struct = "struct"
  val structOpen = "<" + struct + ">"
  val structClose = "</" + struct + ">"

  val target = "target"
  val targetOpen = "<" + target + ">"
  val targetClose = "</" + target + ">"

  val typeTag = "type"
  val typeOpen = "<" + typeTag + ">"
  val typeClose = "</" + typeTag + ">"

  val vertex = "vertex"
  val vertexOpen = "<" + vertex + ">"
  val vertexClose = "</" + vertex + ">"

  val vertexId = "id"
  val vertexIdOpen = "<" + vertexId + ">"
  val vertexIdClose = "</" + vertexId + ">"

  val addrAsVertexId = "vertexId"
  val addrAsVertexIdOpen = "<" + addrAsVertexId + ">"
  val addrAsVertexIdClose = "</" + addrAsVertexId + ">"

  val vertices = "vertices"
  val verticesOpen = "<" + vertices + ">"
  val verticesClose = "</" + vertices + ">"

  val beginEventTimeStep = "beginEventTimeStep"
  val beginEventTimeStepOpen = "<" + beginEventTimeStep + ">"
  val beginEventTimeStepClose = "</" + beginEventTimeStep + ">"

  val endEventTimeStep = "ceaseEventTimeStep"
  val endEventTimeStepOpen = "<" + endEventTimeStep + ">"
  val endEventTimeStepClose = "</" + endEventTimeStep + ">"

  val classSignature = "DsOliCreateXML"

  /**
   * Fetch all vertices and edges from the PTGs over all time steps.
   * Additionally calculate the start and end times for each vertex and edge.
   *
   * @param verticesStore where the vertices with their start and end time get stored
   * @param edgesStore where the edges with their start and end time get stored
   *
   */
  def calculateVerticesAndEdges(verticesStore: HashMap[Long, (DsOliVertex, Long, Long)],
    edgesStore: HashMap[Long, (DsOliDiEdge[DsOliVertex], Long, Long)]): Unit = {
    // Cycle through PTGs for each time step
    for (i <- 0 until ptgs.graphs.length) {
      val graph = ptgs.graphs(i)
      // Process the vertices
      graph.graph.nodes.iterator.foreach {
        node =>
          val vertex = node.value
          // If not present, store start time
          if (!verticesStore.contains(vertex.id)) {
            verticesStore.put(vertex.id, (vertex, i, -1))
          }

          // Always update the end time, so once vertex disappears the end time 
          // is automatically the last time step where the vertex was alive
          val (vertexRef, start, end) = verticesStore.get(vertex.id).get
          verticesStore.put(vertex.id, (vertexRef, start, i))
      }
      // Process the edges
      graph.graph.edges.iterator.foreach {
        edge =>
          val outerEdge = edge.toOuter
          // If not present, store start time
          if (!edgesStore.contains(outerEdge.id)) {
            edgesStore.put(outerEdge.id, (outerEdge, i, -1))
          }

          // Always update the end time, so once outerEdge disappears the end time 
          // is automatically the last time step where the outerEdge was alive
          val (outerEdgeRef, start, end) = edgesStore.get(outerEdge.id).get
          edgesStore.put(outerEdge.id, (outerEdgeRef, start, i))
      }
    }
  }

  /**
   * Generic method for writing a tag and its attributes.
   *
   * @param tag the XML tag
   * @param id the id of the tag
   * @param begin start time
   * @param end end time
   * @param xmlBuffer the StringBuffer to write to
   */
  def writeAttribues(tag: String, id: Long, begin: Long, end: Long, xmlBuffer: StringBuffer): Unit = {
    xmlBuffer.append("<" + tag + " ")

    xmlBuffer.append("id=\"")
    xmlBuffer.append(id)
    xmlBuffer.append("\" ")

    xmlBuffer.append(beginEventTimeStep + "=\"")
    xmlBuffer.append(begin)
    xmlBuffer.append("\" ")

    xmlBuffer.append(endEventTimeStep + "=\"")
    xmlBuffer.append(end)
    xmlBuffer.append("\" >")

  }

  /**
   * Generic method to write an XML element.
   *
   * @param elementOpen opening XML tag
   * @param elementClose closing XML tag
   * @param value the actual value of the the element
   * @param xmlBuffer the StringBuffer to write to
   */
  def writeElement(elementOpen: String, elementClose: String, value: String, xmlBuffer: StringBuffer): Unit = {
    xmlBuffer.append(elementOpen)
    xmlBuffer.append(value)
    xmlBuffer.append(elementClose + "\n")
  }

  /**
   * Write the found vertices to the XML buffer
   *
   * @param xmlBuffer the StringBuffer for storing the XML
   * @param verticesStore the vertices together with their start/end times
   */
  def writeVertices(xmlBuffer: StringBuffer, verticesStore: HashMap[Long, (DsOliVertex, Long, Long)]): Unit = {
    xmlBuffer.append(verticesOpen)
    verticesStore.values.toSeq.sortBy(_._1.id).foreach {
      vals =>
        val (vertexRef, begin, end) = vals
        if (vertexRef.isInstanceOf[DsOliVertexMemory]) {
          val memVertex = vertexRef.asInstanceOf[DsOliVertexMemory]

          writeAttribues("vertex", vertexRef.id, begin, end, xmlBuffer)

          // Write the vertex properties: start/end address, size, type
          writeElement(bAddrOpen, bAddrClose, "0x" + memVertex.bAddr.toHexString, xmlBuffer)
          writeElement(eAddrOpen, eAddrClose, "0x" + memVertex.eAddr.toHexString, xmlBuffer)
          writeElement(sizeOpen, sizeClose, memVertex.vType.size.toString, xmlBuffer)
          writeElement(typeOpen, typeClose, memVertex.vType.vType, xmlBuffer)

          xmlBuffer.append(vertexClose + "\n")
        }
    }
    xmlBuffer.append(verticesClose + "\n")
  }

  /**
   * Write the edge element.
   *
   * @param elemOpen the edge opening tag
   * @param elemClose the edge closing tag
   * @param id the edge id
   * @param addr the start address of the vertex where the edge originates
   * @param offset the offset from addr where the edge actually starts (addr + offset = edge start addr)
   * @param xmlBuffer the StringBuffer to write to
   */
  def writeEdgeElement(elemOpen: String, elemClose: String, id: Long, addr: Long, offset: Long, xmlBuffer: StringBuffer): Unit = {

    xmlBuffer.append(elemOpen)
    xmlBuffer.append(addrAsVertexOpen)
    writeElement(addrAsVertexIdOpen, addrAsVertexIdClose, id.toString, xmlBuffer)
    writeElement(offsetOpen, offsetClose, offset.toString, xmlBuffer)
    xmlBuffer.append(addrAsVertexClose)
    writeElement(addrOpen, addrClose, "0x" + (addr + offset).toHexString, xmlBuffer)
    xmlBuffer.append(elemClose + "\n")

  }

  /**
   * Write the edges into the XML Buffer.
   *
   * @param xmlBuffer the StringBuffer to write to
   * @param edgesStore the edges with their corresponding start/end times
   */
  def writeEdges(xmlBuffer: StringBuffer, edgesStore: HashMap[Long, (DsOliDiEdge[DsOliVertex], Long, Long)]): Unit = {
    xmlBuffer.append(edgesOpen)
    edgesStore.values.toSeq.sortBy(_._1.id).foreach {
      vals =>
        val (edgeRef, begin, end) = vals
        writeAttribues("edge", edgeRef.id, begin, end, xmlBuffer)

        writeEdgeElement(sourceOpen, sourceClose, edgeRef.source.id, edgeRef.sAddr, edgeRef.sOffset, xmlBuffer)
        writeEdgeElement(targetOpen, targetClose, edgeRef.target.id, edgeRef.tAddr, edgeRef.tOffset, xmlBuffer)

        xmlBuffer.append(edgeClose + "\n")
    }
    xmlBuffer.append(edgesClose + "\n")

  }

  /**
   * Write the edges into the XML Buffer.
   *
   * @param xmlBuffer the StringBuffer to write to
   * @param edgesStore the edges with their corresponding start/end times
   */

  def writeCells(xmlBuffer: StringBuffer, cellsStore: HashMap[Long, (DsOliCell, Long, Long)]): Unit = {
    xmlBuffer.append(cellsOpen)
    cellsStore.values.toSeq.sortBy(_._1.id).foreach {
      vals =>
        val (cellRef, begin, end) = vals
        writeAttribues("cell", cellRef.id, begin, end, xmlBuffer)

        writeElement(nameOpen, nameClose, "TBD", xmlBuffer)
        writeElement(typeOpen, typeClose, cellRef.cType.vType, xmlBuffer)
        writeElement(bAddrOpen, bAddrClose, "0x" + cellRef.bAddr.toHexString, xmlBuffer)
        writeElement(eAddrOpen, eAddrClose, "0x" + cellRef.eAddr.toHexString, xmlBuffer)
        writeElement(sizeOpen, sizeClose, cellRef.cType.size.toString, xmlBuffer)
        writeElement(addrAsVertexIdOpen, addrAsVertexIdClose, cellRef.vertexId.toString, xmlBuffer)

        xmlBuffer.append(cellClose + "\n")
    }
    xmlBuffer.append(cellsClose + "\n")

  }

  /**
   * Fetch all strands and cells over all time steps. Additionally
   * calculate the start and end times for each strand and cell.
   *
   * @param cellsStore where the cells with their start and end time get stored
   * @param strandsStore where the strands with their start and end time get stored
   *
   */
  def calculateStrandsAndCells(cellsStore: HashMap[Long, (DsOliCell, Long, Long)],
    strandsStore: HashMap[Long, (DsOliBox, Long, Long)]): Unit = {

    // Cycle through all strands for each time step
    for (i <- 0 until boxSteps.boxSteps.length) {
      // Fetch the current time step
      val boxStep = boxSteps.boxSteps(i)

      // Cycle through all strands for the time step
      boxStep.boxes.foreach {
        boxTuple =>
          val (boxId, box) = boxTuple
          // If not present, store start time
          if (!strandsStore.contains(box.id)) {
            strandsStore.put(box.id, (box, i, -1))
          }

          // Always update the end time, so once box disappears the end time 
          // is automatically the last time step where the box was alive
          val (boxRef, start, end) = strandsStore.get(box.id).get
          strandsStore.put(box.id, (boxRef, start, i))

          // Linear sequence
          box.cells.foreach {
            cell =>
              // If not present, store start time
              if (!cellsStore.contains(cell.id)) {
                cellsStore.put(cell.id, (cell, i, -1))
              }

              // Always update the end time, so once cell disappears the end time 
              // is automatically the last time step where the cell was alive
              val (cellRef, start, end) = cellsStore.get(cell.id).get
              cellsStore.put(cell.id, (cellRef, start, i))

          }

          // Cyclic sequence
          if (box.cycleId != 0) {
            boxStep.cycles.get(box.cycleId).get.cells.foreach {
              cell =>
                // If not present, store start time
                if (!cellsStore.contains(cell.id)) {
                  cellsStore.put(cell.id, (cell, i, -1))
                }

                // Always update the end time, so once cell disappears the end time 
                // is automatically the last time step where the cell was alive
                val (cellRef, start, end) = cellsStore.get(cell.id).get
                cellsStore.put(cell.id, (cellRef, start, i))

            }
          }
      }
    }

  }

  def writeStrands(xmlBuffer: StringBuffer, strandsStore: HashMap[Long, (DsOliBox, Long, Long)]): Unit = {
    xmlBuffer.append(strandsOpen)
    strandsStore.values.toSeq.sortBy(_._1.id).foreach {
      vals =>
        val (strandRef, begin, end) = vals

        writeAttribues("strand", strandRef.id, begin, end, xmlBuffer)

        writeElement(cellTypeOpen, cellTypeClose, strandRef.cType.vType, xmlBuffer)
        writeElement(linkageOffsetOpen, linkageOffsetClose, strandRef.offset.toString, xmlBuffer)

        xmlBuffer.append(cellSequencesOpen)
        xmlBuffer.append("<cellSequence ")

        xmlBuffer.append(beginEventTimeStep + "=\"")
        xmlBuffer.append(begin)
        xmlBuffer.append("\" ")

        xmlBuffer.append(endEventTimeStep + "=\"")
        xmlBuffer.append(end)
        xmlBuffer.append("\">\n")

        xmlBuffer.append(linearSequenceOpen)
        for (i <- 0 until strandRef.cells.length) {
          xmlBuffer.append("<cellRef ")
          xmlBuffer.append(cellPosition + "=\"" + i + "\" " + cellId + "=\"" + strandRef.cells(i).id + "\"/>")
        }
        xmlBuffer.append(linearSequenceClose + "\n")

        xmlBuffer.append(cyclicSequenceOpen)
        if (strandRef.cycleId != 0) {
          val cycle = boxSteps.boxSteps(begin.toInt).cycles.get(strandRef.cycleId).get
          val cycleEntryCell = cycle.cells.find(_.id == strandRef.cycleEntryPoint).get
          // Get the entry cell into the cycle
          val cycleEntryCellIndex = boxesCreator.getCellIndexFromList(cycleEntryCell, cycle.cells).get
          // Get the slice up until (not including) the entry cell
          val firstSlice = cycle.cells.slice(0, cycleEntryCellIndex)
          // Get the slice from the entry to the end
          val secondSlice = cycle.cells.slice(cycleEntryCellIndex, cycle.cells.size)
          // Start by printing the second slice, which is the entry point. 0 is the index where we start counting
          xmlBuffer.append(printCyclicSlice(secondSlice, 0))
          // Now print the remaining part of the cycle, which starts at index secondSlice.length
          xmlBuffer.append(printCyclicSlice(firstSlice, secondSlice.length))
        }
        xmlBuffer.append(cyclicSequenceClose + "\n")
        xmlBuffer.append(cellSequenceClose + "\n")

        xmlBuffer.append(cellSequencesClose + "\n")
        xmlBuffer.append(strandClose + "\n")
    }
    xmlBuffer.append(strandsClose + "\n")
  }

  def printCyclicSlice(cells: ListBuffer[DsOliCell], index: Int): String = {
    val xmlBuffer = new StringBuffer()
    var itIndex = index
    cells.foreach {
      cell =>
          xmlBuffer.append("<cellRef ")
          xmlBuffer.append(cellPosition + "=\"" + itIndex + "\" " + cellId + "=\"" + cell.id + "\"/>")
        itIndex += 1
    }
    xmlBuffer.toString
  }


  /**
   * Create an XML representation of all the information created by DSI during the analysis.
   * This includes low level information about PTGs (e.g. memory vertices and edges including
   * their lifetime), cells,  representation of the SG, FSG and AGG.
   *
   * The XML is collected in a StringBuffer and gets written out as a whole after all information
   * is gathered.
   *
   */
  def createXML(): Unit = {
    val funSignature = classSignature + "::createXML: "
    val xmlBuffer = new StringBuffer();

    // Stores the vertices together with their start/end times
    val verticesStore = new HashMap[Long, (DsOliVertex, Long, Long)]()
    // Stores the edges together with their start/end times
    val edgesStore = new HashMap[Long, (DsOliDiEdge[DsOliVertex], Long, Long)]()
    // Stores the cells together with their start/end times
    val cellsStore = new HashMap[Long, (DsOliCell, Long, Long)]()
    // Stores the strands together with their start/end times
    val strandsStore = new HashMap[Long, (DsOliBox, Long, Long)]()

    // Start of the XML file
    xmlBuffer.append("<?xml version=\"1.0\"?>\n")
    xmlBuffer.append(ptgOpen + "\n")

    calculateVerticesAndEdges(verticesStore, edgesStore)
    calculateStrandsAndCells(cellsStore, strandsStore)

    writeVertices(xmlBuffer, verticesStore)
    writeEdges(xmlBuffer, edgesStore)
    writeCells(xmlBuffer, cellsStore)
    writeStrands(xmlBuffer, strandsStore)

    // End of the XML file
    xmlBuffer.append(ptgClose + "\n")

    var dirPath = DsOliPathUtils.getPath
    val writer = new PrintWriter(dirPath + "dsi-" + DsOliPathUtils.getXMLFile + "-result.xml")
    writer.write(xmlBuffer.toString)
    writer.close()
  }

}