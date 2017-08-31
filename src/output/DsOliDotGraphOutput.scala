
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
 * DsOliDotGraphOutput.scala created on Oct 23, 2014
 *
 * Description: Produces the DOT files
 */
package output

import pointstograph.DsOliGraph
import scalax.collection.io.dot._
import pointstograph.DsOliDiEdge
import pointstograph.DsOliVertex
import pointstograph.DsOliVertexMemory
import extlogger.DsOliLogger
import pointstograph.DsOliVertexNull
import pointstograph.DsOliVertexUndef
import scalax.collection.GraphEdge.DiEdge
import pointstograph.DsOliVertexPredefined
import scala.util.control.Breaks._
import pointstograph.ITypeDB
import pointstograph.DsOliType
import pointstograph.FieldType._
import scalax.collection.Graph
import boxcalculation.DsOliBoxStep
import boxcalculation.DsOliBox
import boxcalculation.DsOliBox.BoxId
import scala.collection.mutable.ListBuffer
import boxcalculation.DsOliCell
import boxcalculation.DsOliCycle
import entrypoint.DsOliFeatureSet
import entrypoint.Feature
import util.DsOliAddressUtils
import entrypoint.DsOliEPT
import entrypoint.Feature._
/**
 * @author DSI
 *
 */
class DsOliDotGraphOutput(typeDB: ITypeDB) extends IDsOliGraphOutput {

  val classSignature = "DsOliDotGraphOutput::"

  /**
   * HTML for the vertices
   */
  val tableStart = """<TABLE BORDER="0" COLOR="black" CELLBORDER="1" CELLSPACING="0">"""
  val tableLastInsertedStart = """<TABLE BORDER="2" COLOR="red" CELLBORDER="1" CELLSPACING="0">"""
  val tableEnd = "</TABLE>"
  val rowStart = "<TR>"
  val rowEnd = "</TR>"
  val tableHead = rowStart + """<TD>Field name</TD><TD>Data type</TD><TD>Size</TD><TD>Address</TD><TD>Boxes</TD>""" + rowEnd

  /**
   * Template for a TD field of the table.
   * Everything ending in Repl will be replaced during table creation
   * with a Search and Replace command.
   */
  val color = "COLOR"
  val port = "PORT"
  val colSpan = "COLSPAN"
  val cellData = "CELLDATA"
  val colorRepl = "%" + color + "%"
  val portRepl = "%" + port + "%"
  val colSpanRepl = "%" + colSpan + "%"
  val cellDataRepl = "%" + cellData + "A%"
  val cell = "<TD BGCOLOR=\"" + colorRepl + "\" " + colSpanRepl + " " + portRepl + " ALIGN=\"left\">" + cellDataRepl + "</TD>"

  /**
   * Color values
   */
  val colorValStructBegEnd = "FF974B"
  val colorValPtrBegEnd = "00FFFF"
  val colorValStructField = "EEDDCC"
  val colorValPtrToNullUndef = "CCDDEE"
  val colorValPredfinedVertex = "37BAAC"
  val colorValStandardEdge = "000000"
  val colorValNewestEdge = "FF0000"
  val colorValCompilerPadding = "FFFF00"

  /**
   * Ports for DOT
   */
  val portHead = "head"
  val portBody = "body"

  /**
   * Nested elements are indicated with this character
   */
  val paddingItem = "+"

  /**
   * Settings for the DOT header
   */
  val dotHeader = DotRootGraph(
    directed = true,
    id = Some("G"),
    kvList = Seq.empty)

  // The graph we are currently operating of
  var currentGraph: DsOliGraph = null
  // The last used edge (used for highlighting the edge)
  var lastEdge: DsOliDiEdge[DsOliVertex] = null
  // Optional strands
  var boxesOpt: Option[DsOliBoxStep] = None
  // Entry pointer tags if present
  var epts: DsOliFeatureSet = null
  var i = 0
  var u = 0
  // Indicate if currently an artificial event is written
  var isArtificial = false

  /**
   * Recursively create a HTML representation of a type
   *
   * @param innerNode the node (vertex) of the graph to deflate
   * @param vType the vertex type
   * @param baseAddress the current base address of the inspected type (will change during recursion)
   * @param sumSizesFromStartAddr the sum of all sizes of the types processed thus far
   * @param strands Option set of strands
   * @param cycles Option set of cycles
   * @param padding padding for nested structs
   * @return Tuple with string representation and the sum of all added types
   */
  def deflateType(innerNode: scalax.collection.Graph[DsOliVertex, DsOliDiEdge]#NodeT, vType: DsOliType,
    baseAddress: Long, sumSizesFromStartAddr: Long, boxes: Option[Set[DsOliBox]],
    cycles: Option[Set[DsOliCycle]], padding: String = paddingItem): (String, Long) = {
    val funSignature = classSignature + "deflateType: "
    DsOliLogger.debug(funSignature + "entered. Processing id: " + innerNode.value.id.toString +
      ",vType : " + vType + ", baseAddress: " + baseAddress + ", padding: " + padding)
    var rows = ""

    var tmpSumSizesFromStartAddr = sumSizesFromStartAddr
    var paddingSize: Long = 0
    // Test if the memory has fields
    if (vType != null && vType.fields != null && vType.fields.length != 0) {
      DsOliLogger.debug(funSignature + "if passed");

      // Cycle through all fields of the type
      vType.fields.foreach {
        field =>

          DsOliLogger.debug(funSignature + "inspecting field: " + field)
          var fieldColor = colorValStructField
          val offset = (baseAddress + field.vOffset)

          // Fetch the strands and cycles for the current field
          DsOliLogger.debug(funSignature + "calling findBoxesAndCyclesForCell")
          val boxesCyclesOpt = findBoxesAndCyclesForCell(offset, offset + field.vSize - 1, boxes, cycles)
          DsOliLogger.debug(funSignature + "return from findBoxesAndCyclesForCell")

          // Create a string representation of the strands and cycles
          var boxCycleStr = if (boxesCyclesOpt.isDefined) {
            DsOliLogger.debug(funSignature + "boxesCyclesOpt is defined")
            val (boxes, cycles) = boxesCyclesOpt.get
            createBoxCycleList(boxes.toList.sortBy(_.id), cycles)
          } else {
            DsOliLogger.debug(funSignature + "boxesCyclesOpt not defined")
            ""
          }

          // Create a port for a compound field used by DOT to connect
          // edges
          var port = if (field.fType != Compound) offset.toString else ""

          // Do we have a pointer, if yes add do some coloring
          // in depending on the value of the pointer (undef, NULL, assigned)
          if (field.fType == Pointer) {

            fieldColor = colorValPtrBegEnd
            // Test if there is an outgoing edge for this particular offset 
            // that points to NULL/UNDEF
            breakable {
              innerNode.outgoing.foreach {
                edge =>
                  if (edge.edge.sAddr + edge.edge.sOffset == offset &&
                    edge.edge.to.value.isInstanceOf[DsOliVertexNull]) {
                    fieldColor = colorValPtrToNullUndef
                    break
                  } else if (edge.edge.sAddr + edge.edge.sOffset == offset &&
                    edge.edge.to.value.isInstanceOf[DsOliVertexUndef]) {
                    fieldColor = "FF0000"
                    break
                  }
              }
            }
          }

          paddingSize = 0
          if (tmpSumSizesFromStartAddr < offset) {
            paddingSize = offset - tmpSumSizesFromStartAddr
            rows += rowStart +
              createCell(cellData = padding + "untyped", colorValue = colorValCompilerPadding) +
              createCell(cellData = "byte", colorValue = colorValCompilerPadding) +
              createCell(cellData = paddingSize.toString, colorValue = colorValCompilerPadding) +
              createCell(cellData = (offset - paddingSize).toHexString, colorValue = colorValCompilerPadding) +
              createCell(cellData = "", colorValue = colorValCompilerPadding) +
              rowEnd
          }

          tmpSumSizesFromStartAddr += paddingSize

          rows += rowStart +
            createCell(cellData = padding + field.name, portValue = port + "in", colorValue = fieldColor) +
            createCell(cellData = field.cType, colorValue = fieldColor) +
            createCell(cellData = field.vSize.toString, colorValue = fieldColor) +
            createCell(cellData = offset.toHexString, colorValue = fieldColor) +
            createCell(cellData = boxCycleStr, portValue = port + "out", colorValue = fieldColor) +
            rowEnd

          // Inspect compound fields
          if (field.fType == Compound) {
            val newType = typeDB.getTypeObject(field.cType)
            if (newType.isDefined) {

              // Recurse
              val (tmpRows, retSumSizesFromStartAddr) = deflateType(innerNode, newType.get, baseAddress + field.vOffset,
                tmpSumSizesFromStartAddr, boxes, cycles, padding + paddingItem)
              rows += tmpRows
              tmpSumSizesFromStartAddr = retSumSizesFromStartAddr
            }
          } else {
            tmpSumSizesFromStartAddr += field.vSize
          }
      }

    } else {
      DsOliLogger.debug(funSignature + "just adding to the padding: tmpSumSizesFromStartAddr: " + tmpSumSizesFromStartAddr.toHexString + " vType.size: " + vType.size)
      tmpSumSizesFromStartAddr += paddingSize + vType.size
    }

    return (rows, tmpSumSizesFromStartAddr)
  }

  /**
   * Highlight the last written/used edge
   *
   * @param innerEdge the edge to test
   * @return string with color for edge
   */
  def calculateEdgeColor(innerEdge: scalax.collection.Graph[DsOliVertex, DsOliDiEdge]#EdgeT): String = {
    val edge = innerEdge.edge
    // Highlight the last inserted edge
    val edgeColor = if (lastEdge != null &&
      lastEdge.sAddr == edge.sAddr && lastEdge.sOffset == edge.sOffset &&
      lastEdge.tAddr == edge.tAddr && lastEdge.tOffset == edge.tOffset) {
      colorValNewestEdge
    } else {
      colorValStandardEdge
    }
    return edgeColor
  }

  /**
   * Create the attributes for the edge:
   * - Add the label to the edge if label is not empty.
   * - Mark the edge invisible if vertex is not a memory vertex (i.e., skip edges to NULL/undef)
   *
   * @return list with the attribute
   */
  def calculateEdgeAttributes(vertex: DsOliVertex, label: String, edgeColor: String): List[scalax.collection.io.dot.DotAttr] = {
    vertex match {
      case v: DsOliVertexMemory =>

        if (label.nonEmpty) {
          List(DotAttr("color", "\"#" + edgeColor + "\""), DotAttr("label", label))
        } else {
          List(DotAttr("color", "\"#" + edgeColor + "\""))
        }
      case _ =>
        List(DotAttr("style", "invis"))
    }
  }

  // Indicates how many features to print
  val featureEnd = 20

  /**
   * Create a string representation of the artificial features
   * for a given time step
   *
   * @param ept the entry pointer tag
   * @param i the time step
   * @return the string representation for the artificial features
   */
  def buildArtString(ept: DsOliEPT, i: Int): String = {
    var retStr = ""
    if (ept.artificialQfs.contains(i)) {
      ept.artificialQfs.get(i).get.foreach {
        artFeature =>
          retStr += ":" + Feature.featureMapping(artFeature)
      }
    }
    retStr
  }

  /**
   * Create the final HTML table from the real and artificial
   * feature entries.
   *
   * @param realFeatureString the real features
   * @param artFeatureString the artificial features
   * @return the table containing both the real and artificial features
   */
  def createRealArtFeatureTable(realFeatureString: String, artFeatureString: String): String = {
    var mainFeatures = "<TD BGCOLOR=\"#66CC33\">real</TD>" + realFeatureString
    var artFeatures = "<TD BGCOLOR=\"#D8D8D8\">artificial</TD>" + artFeatureString
    "<TABLE border=\"0\" frame=\"void\"><TR>" + mainFeatures + "</TR><TR>" + artFeatures + "</TR></TABLE>"
  }

  /**
   * Create the table entry for the feature and the artificial feature string
   *
   * @return tuple of main and artificial table entries
   */
  def createRealArtFeatureEntry(feature: String, artFeatureString: String): (String, String) = {
    var mainFeatures = "<TD BGCOLOR=\"#66CC33\">" + feature + "</TD>"
    var artFeatures = "<TD BGCOLOR=\"#D8D8D8\">" + artFeatureString + "</TD>"
    return (mainFeatures, artFeatures)
  }

  /**
   * Create the actual HTML table for the EPT
   *
   * @param i the current time step
   * @param ept the EPT to check
   * @param forSur indicate if we deal with a surrogate EPT
   */
  def buildEPTEntry(i: Int, ept: DsOliEPT, forSur: Boolean = false): String = {
    val funSignature = classSignature + "buildEPTEntry: "
    DsOliLogger.debug(funSignature + "entered: " + ept);
    var retStr = ""
    retStr += "<TR><TD>EPT</TD><TD>Up to " + featureEnd + " previous features</TD><TD>Feature</TD></TR>"

    // Are there any previous features?
    val prevFeature = if ((i - ept.creationTime + u) - 1 < 0) {
      "non existant"
    } else {
      // Previous features are available

      // Use the artificial time step or the original time step
      var end = if (ept.artificialQfs.contains(i) && isArtificial) {
        i - ept.creationTime.toInt + u
      } else {
        i - ept.creationTime.toInt
      }

      // Calculate how many features should be printed:
      // if "look back" is negative start from beginning, else
      // rewind e to the "look back"
      var e = if ((end - featureEnd) < 0) 0 else end - featureEnd

      var mainFeatures = ""
      var artFeatures = ""
      // Iterate over all elements starting from the "look back" index e
      // stopping at the end
      while (e < end) {

        // Check if we need to look at artificial events as well
        val index = if (ept.artificialQfs.contains(i) && isArtificial) {
          // Need to cut, to get the right index for artificials
          if (e >= (i - ept.creationTime)) {
            e - (i - ept.creationTime.toInt)
          } else e
        } else e

        // Create the actual feature, again obey artificial events/features.
        // String for main and artificial features are created separately.
        val (tmpMain, tmpArt) = if (ept.artificialQfs.contains(i) && isArtificial) {
          if (e >= (i - ept.creationTime))
            createRealArtFeatureEntry("artificial", Feature.featureMapping(ept.artificialQfs.get(i).get(index)))
          else
            createRealArtFeatureEntry(Feature.featureMapping(ept.Qf(index)), buildArtString(ept, e + ept.creationTime.toInt))
        } else {
          createRealArtFeatureEntry(Feature.featureMapping(ept.Qf(index)), buildArtString(ept, e + ept.creationTime.toInt))
        }

        // Save the main and artificial strings
        mainFeatures += tmpMain
        artFeatures += tmpArt

        e += 1
      }

      // Glue everything together
      createRealArtFeatureTable(mainFeatures, artFeatures)

    }

    // Now create the current feature
    val featureTable =
      if (ept.artificialQfs.contains(i) && isArtificial) {
        val (main, art) = createRealArtFeatureEntry("artificial", Feature.featureMapping(ept.artificialQfs.get(i).get(u)))
        createRealArtFeatureTable(main, art)
      } else {
        val (main, art) = createRealArtFeatureEntry(Feature.featureMapping(ept.Qf((i - ept.creationTime).toInt)), buildArtString(ept, i))
        createRealArtFeatureTable(main, art)
      }
    retStr += "<TR><TD> ept.id = " + ept.id + "; Aup.bAddr = " + ept.Aup.bAddr.toHexString + "; Aup.linkageOffset = " + ept.Aup.linkageOffset +
      "v.id = " + ept.epVertexId + ": i = " + i + " index = " + (i - ept.creationTime).toInt + "; ept.creationTime = " + ept.creationTime +
      " ept.Qf.length = " + ept.Qf.length + "</TD><TD>" + prevFeature +
      "</TD><TD>" + featureTable + "</TD></TR>"

    retStr
  }

  /**
   * Check through the surrogate EPTs and check if they are associated with the vertex
   * through the Aup cell.
   *
   * @param targetVertex the vertex to search for
   * @return string representation of the EPT
   */
  def calculateSurrogateEPTForVertex(targetVertex: DsOliVertexMemory): String = {
    val funSignature = classSignature + "calculateSurrogateEPTForVertex: "
    DsOliLogger.debug(funSignature + "entered")
    var retStr = ""
    if (this.epts == null) return retStr
    this.epts.features.foreach {
      ept =>

        // Take artificial events into account for calculating end and index
        val (end, index) = if (ept.artificialQfs.contains(i) && isArtificial) {
          DsOliLogger.debug(funSignature + "artQfs && isArtificial in end calculation")
          (ept.creationTime + ept.Qf.size + ept.artificialQfs.get(i).get.size, i + u)
        } else {
          DsOliLogger.debug(funSignature + "else in end calculation")
          (ept.creationTime + ept.Qf.size, i)
        }

        DsOliLogger.debug(funSignature + "isArtificial = " + isArtificial)
        DsOliLogger.debug(funSignature + "ept.artificialQfs.contains(i) = " + ept.artificialQfs.contains(i))
        DsOliLogger.debug(funSignature + "index = " + index + ", i = " + i + ", u = " + u + ", end = " + end + ".")

        // Check if Aup of the EPT lies within the target vertex and check if the current time step lies within
        // the lifetime of the EPT
        if (ept.epVertexId == 0
          && DsOliAddressUtils.addressInRange(targetVertex.bAddr, targetVertex.eAddr, ept.Aup.bAddr, ept.Aup.eAddr)
          && ept.creationTime <= index && index < end) {

          DsOliLogger.debug(funSignature + "ept seems alive.")
          DsOliLogger.debug(funSignature + "calling buildEPTEntry.")

          retStr += buildEPTEntry(i, ept, true)

        } else if (ept.epVertexId == 0
          && DsOliAddressUtils.addressInRange(targetVertex.bAddr, targetVertex.eAddr, ept.Aup.bAddr, ept.Aup.eAddr)
          && ept.creationTime <= index && index >= end) {

          DsOliLogger.debug(funSignature + "ept is expired")

          retStr += "<TR><TD> ept.id = " + ept.id + "; Aup.bAddr = " + ept.Aup.bAddr.toHexString + "; Aup.linkageOffset = " + ept.Aup.linkageOffset +
            "v.id = " + ept.epVertexId + ": i = " + index + " index = " + (index - ept.creationTime).toInt + "; ept.creationTime = " + ept.creationTime +
            " ept.Qf.length = " + ept.Qf.length + "</TD><TD> expired" +
            "</TD><TD>expired</TD></TR>"
        }
    }
    retStr
  }

  /**
   * Build the EPT features for an edge
   *
   * @param sourceVertex the source vertex of the edge
   * @param innerEdge the edge
   * @return string representing the table for the features in HTML
   */
  def calculateFeaturesForEdge(sourceVertex: DsOliVertexMemory, innerEdge: scalax.collection.Graph[DsOliVertex, DsOliDiEdge]#EdgeT): String = {
    val funSignature = classSignature + "calculateFeaturesForEdge: "
    val edge = innerEdge.edge
    var retStr = ""

    // Skip EPTs if not set
    if (this.epts == null) return retStr

    // Cycle through the features of the ep
    this.epts.features.foreach {
      ept =>

        // If the EPT is associated with the current given vertex and we are 
        // dealing with the edge associated with the EPT and the current time
        // step lies within the recorded feature trace of the EPT
        if (ept.epVertexId == sourceVertex.id && sourceVertex.bAddr + ept.oep == edge.sAddr + edge.sOffset &&
          ept.creationTime <= i && i < ept.creationTime + ept.Qf.size) {
          DsOliLogger.debug(funSignature + "calling buildEPTEntry.")
          retStr += buildEPTEntry(i, ept)
        }
    }

    // Return the table
    if (retStr == "") retStr else "<<TABLE>" + retStr + "</TABLE>>"
  }

  /**
   * Transform the edges of the graph to DOT output
   *
   * @param innerEdge the edge to transform
   * @return Option the DOT graph and the DOT statement for the edge
   */
  def edgeTransformer(innerEdge: scalax.collection.Graph[DsOliVertex, DsOliDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {

    val edge = innerEdge.edge

    // Do we need to highlight the current edge?
    val edgeColor = calculateEdgeColor(innerEdge)

    val sourceVertex = edge.from.value.asInstanceOf[DsOliVertexMemory]

    // Do we have any features for EPT to display?
    val label = calculateFeaturesForEdge(sourceVertex, innerEdge)

    // Add the features to edge or possibly hide the edge based on the target vertex
    val attributes = calculateEdgeAttributes(edge.to.value, label, edgeColor)

    // Calculate the port to start from: normally start address, if 
    // no fields, use the head port
    val fromPort = if (sourceVertex.vType.fields == null) {
      portHead
    } else {
      (edge.sAddr + edge.sOffset).toString
    }

    // Calculate the port to connect to: always the target address
    val toPort = (edge.tAddr + edge.tOffset).toString

    // Create the DOT statement
    Some(dotHeader,
      DotEdgeStmt(
        "\"" + edge.from.value.id.toString + "\":\"" + fromPort + "out:e\"",
        "\"" + edge.to.value.id.toString + "\":\"" + toPort + "in:w\"",
        attributes))
  }

  /**
   * Create a cell of table by replacing the Repl values with the passed
   * in values.
   *
   * @param cellData the data of the cell
   * @param portValue the port used to connect edges
   * @param colSpanValue how many columns are connected
   * @param colorValue the color value
   * @return  the created cell HTML representation
   */
  def createCell(cellData: String, portValue: String = "", colSpanValue: String = "1", colorValue: String = "000000"): String = {
    cell.replace(colorRepl, "#" + colorValue)
      .replace(colSpanRepl, colSpan + "=\"" + colSpanValue + "\"")
      .replace(portRepl, port + "=\"" + portValue + "\"")
      .replace(cellDataRepl, cellData)
  }

  /**
   * Fetch all strands and cycles for a given cell,
   * i.e., its address
   *
   * @param bAddr the memory start address of the cell
   * @param eAddr the memory end address of the cell
   * @param boxes Option the set of strands to search in
   * @param cycles Option the set of cycles to search in
   * @return Option tuple with the set of found strands and cycles
   */
  def findBoxesAndCyclesForCell(bAddr: Long, eAddr: Long,
    boxes: Option[Set[DsOliBox]], cycles: Option[Set[DsOliCycle]]): Option[(Set[DsOliBox], Set[DsOliCycle])] = {
    val funSignature = classSignature + "findBoxesAndCyclesForCell: "

    DsOliLogger.debug(funSignature + "entered. bAddr: " + bAddr.toHexString + ", eAddr: " + eAddr.toHexString)
    DsOliLogger.debug(funSignature + "boxes.isDefined: " + boxes.isDefined + "cycles.isDefined: " + cycles.isDefined)

    return if (boxes.isDefined && cycles.isDefined) {

      var boxesForVertex = Set[DsOliBox]()
      var cyclesForVertex = Set[DsOliCycle]()

      // Cycle through all strands
      boxes.get.foreach {
        box =>
          DsOliLogger.debug(funSignature + "testing box: " + box)

          // Check linear strand part first
          box.cells.foreach {
            cell =>
              DsOliLogger.debug(funSignature + "cell.bAddr: " + cell.bAddr + ", bAddr: " + bAddr +
                " cell.eAddr: " + cell.eAddr + ", eAddr: " + eAddr)
              if (cell.bAddr == bAddr && cell.eAddr == eAddr) {
                DsOliLogger.debug(funSignature + "found box for type: " + box)
                if (!boxesForVertex.contains(box)) boxesForVertex += box
              }
          }

          // Then check cyclic part
          if (box.cycleId != 0) {
            val cycleOpt = cycles.get.find(cycle => cycle.id == box.cycleId)
            if (cycleOpt.isDefined) {
              cycleOpt.get.cells.foreach {
                cell =>
                  if (cell.bAddr == bAddr && cell.eAddr == eAddr) {
                    if (!boxesForVertex.contains(box)) boxesForVertex += box
                  }
              }
            } else {
              DsOliLogger.debug(funSignature + "no cycle for cycleId: " + box.cycleId)
            }

          }
      }

      // Check exclusive cycles
      cycles.get.foreach {
        cycle =>
          DsOliLogger.debug(funSignature + "testing cycle: " + cycle)
          cycle.cells.foreach {
            cell =>
              if (cell.bAddr == bAddr && cell.eAddr == eAddr) {
                DsOliLogger.debug(funSignature + "found cycle for type: " + cycle)
                if (!cyclesForVertex.contains(cycle)) cyclesForVertex += cycle
              }
          }
      }
      Some((boxesForVertex, cyclesForVertex))
    } else {
      None
    }
  }

  /**
   * Find all the strands, cycles and cells the given vertex
   *
   * @param vertex the vertex to search for
   * @return Option tuple of a set of strands, cycles and cells for the vertex
   */
  def findBoxesCyclesAndCellsForVertex(vertex: DsOliVertexMemory): Option[(Set[DsOliBox], Set[DsOliCycle], Set[DsOliCell])] = {
    val funSignature = classSignature + "findBoxesCyclesAndCellsForVertex: "
    DsOliLogger.debug(funSignature + "entered. vertex: " + vertex + " this.boxesOpt.isDefined: " + this.boxesOpt.isDefined)

    return if (this.boxesOpt.isDefined) {
      var cellsForVertex = Set[DsOliCell]()
      var boxesForVertex = Set[DsOliBox]()
      var cyclesForVertex = Set[DsOliCycle]()
      val boxes = boxesOpt.get
      DsOliLogger.debug(funSignature + "boxes.boxes: " + boxes.boxes.size)

      // Check through all strands
      boxes.boxes.foreach {
        boxTuple =>
          val (boxId, box) = boxTuple
          DsOliLogger.debug(funSignature + "checking box: " + boxId)
          DsOliLogger.debug(funSignature + "checking box.cells: " + box.cells.size)

          // First the linear sequence
          box.cells.foreach {
            cell =>
              DsOliLogger.debug(funSignature + "cell.vertexId:" + cell.vertexId + " == vertex.id: " + vertex.id)

              // Is the cell associated with the vertex
              if (cell.vertexId == vertex.id) {
                DsOliLogger.debug(funSignature + "found cell for vertex in box: " + cell)

                // Only record once
                if (!cellsForVertex.contains(cell)) cellsForVertex += cell
                if (!boxesForVertex.contains(box)) boxesForVertex += box

              }
          }

          DsOliLogger.debug(funSignature + "done with box.cells")
          DsOliLogger.debug(funSignature + "checking box.cycleId: " + box.cycleId)

          // Check the cyclic part
          if (box.cycleId != 0) {
            val cycle = boxes.cycles.get(box.cycleId).get
            cycle.cells.foreach {
              cell =>

                // Is the cell associated with the vertex
                if (cell.vertexId == vertex.id) {
                  DsOliLogger.debug(funSignature + "found cell for vertex in cycle: " + cell)

                  // Only record once
                  if (!cellsForVertex.contains(cell)) cellsForVertex += cell
                  if (!boxesForVertex.contains(box)) boxesForVertex += box
                  if (!cyclesForVertex.contains(cycle)) cyclesForVertex += cycle
                }
            }
          }
          DsOliLogger.debug(funSignature + "done with box.cycleId")
      }

      DsOliLogger.debug(funSignature + "checking exclusive cycle boxes: " + boxes.cycles.size)
      // Check through the exclusive cycles
      boxes.cycles.foreach {
        cycleTuple =>
          val (cycleId, cycle) = cycleTuple
          cycle.cells.foreach {
            cell =>
              if (cell.vertexId == vertex.id) {
                DsOliLogger.debug(funSignature + "found cell for vertex in cycle: " + cell)
                if (!cellsForVertex.contains(cell)) cellsForVertex += cell
                if (!cyclesForVertex.contains(cycle)) cyclesForVertex += cycle
              }
          }
      }
      DsOliLogger.debug(funSignature + "done with exclusive cycle boxes")

      Some((boxesForVertex, cyclesForVertex, cellsForVertex))
    } else {
      DsOliLogger.debug(funSignature + "box not defined")
      None
    }
  }

  /**
   * Create a string representing the strands and cycles
   *
   * @param boxes the strands to list
   * @param cycles the cycles to list
   * @return the string representation
   */
  def createBoxCycleList(boxes: List[DsOliBox], cycles: Set[DsOliCycle]): String = {
    val funSignature = classSignature + "createBoxCycleList: "
    var boxesForVertex = ""
    boxes.foreach {
      box =>
        // If the box has a cycle id and this vertex participates in this cycle, then add this information
        val cycleId =
          if (box.cycleId != 0 && cycles.exists(_.id == box.cycleId)) {
            ":" + box.cycleId
          } else {
            ""
          }
        boxesForVertex += box.id + cycleId + ","
    }
    DsOliLogger.debug(funSignature + "found the following ids: " + boxesForVertex + ".")
    // Chop last comma
    boxesForVertex.slice(0, boxesForVertex.length() - 1)
  }

  /**
   * Transform a node into HTML
   *
   * @param innerNode the node to transform
   * @return the HTML string
   */
  def nodeToHTML(innerNode: scalax.collection.Graph[DsOliVertex, DsOliDiEdge]#NodeT): String = {
    val funSignature = classSignature + "nodeToHTML: "

    // Standard highlighting
    var tableDefinition = tableStart

    // Try to highlight the vertices that got 
    // connected by the last written edge. This might
    // fail, as intermediate artificial events can
    // interrupt.
    if (lastEdge != null) {
      DsOliLogger.debug(funSignature + "trying to find lastEdge: " + lastEdge)
      try {
        var lastEdgeInner = currentGraph.graph.get(lastEdge)

        // Highlight table in red, if current node is either source or target of the last written edge
        if (lastEdgeInner != null && (innerNode.value == lastEdgeInner.edge.to.value || innerNode.value == lastEdgeInner.edge.from.value)) {
          tableDefinition = tableLastInsertedStart
        }

      } catch {
        case e: Exception =>
          DsOliLogger.debug(funSignature + "trying to find lastEdge failed. Properly  because of intermediate artificial events: " + lastEdge)
      }
    }

    // Create the table
    val table = "<" +
      tableDefinition +
      rowStart + createCell(cellData = innerNode.value.id.toString, colSpanValue = "5", colorValue = "FFFFFF") + rowEnd +
      tableHead +
      (

        // Create different content depending on the type of memory vertex
        innerNode.value match {
          case v: DsOliVertexMemory =>
            var rows = ""
            var color = colorValPtrBegEnd

            // Find all the cells which were created in the box calculation for this vertex
            val cellsForVertexOpt = findBoxesCyclesAndCellsForVertex(v)

            // Create a string representation of the strands and cycles
            var boxesForVertex = ""
            val (boxesDeflate, cyclesDeflate) = if (cellsForVertexOpt.isDefined) {
              val (boxes, cycles, _) = cellsForVertexOpt.get
              boxesForVertex = createBoxCycleList(boxes.toList.sortBy(_.id), cycles)
              (Some(boxes), Some(cycles))
            } else {
              (None, None)
            }
            DsOliLogger.debug(funSignature + "deflateType boxesForVertex: " + boxesForVertex + ".")

            // Test if the memory has fields
            if (v.vType != null && v.vType.fields != null && v.vType.fields.length != 0) {
              color = colorValStructBegEnd
              val (tmpRows, tmpSumSizesFromStartAddr) = deflateType(innerNode, v.vType, v.bAddr, v.bAddr, boxesDeflate, cyclesDeflate, padding = "")
              rows += tmpRows

              // Check if we found a mismatch between the summed sizes
              // and the actual end address -> treat this as padding inserted by the compiler
              if (tmpSumSizesFromStartAddr < v.eAddr) {
                val paddingSize = v.eAddr - tmpSumSizesFromStartAddr //+ 1
                rows += rowStart +
                  createCell(cellData = "untyped", colorValue = colorValCompilerPadding) +
                  createCell(cellData = "byte", colorValue = colorValCompilerPadding) +
                  createCell(cellData = paddingSize.toString, colorValue = colorValCompilerPadding) +
                  createCell(cellData = (v.eAddr - paddingSize).toHexString, colorValue = colorValCompilerPadding) +
                  createCell(cellData = "", colorValue = colorValCompilerPadding) +
                  rowEnd
              }

            } else {
              // Test if there is exactly one outgoing edge and if this edge is pointing to NULL/UNDEF
              if (innerNode.outgoing.size == 1 && innerNode.outgoing.head.edge.to.value.isInstanceOf[DsOliVertexPredefined])
                color = colorValPtrToNullUndef
            }

            // Calculate any surrogate entry pointer tags for a vertex
            val surEPTs = calculateSurrogateEPTForVertex(v)

            // Build the HTML representation for the vertex
            rowStart +
              createCell(cellData = "memory region", portValue = portHead, colorValue = color) +
              createCell(cellData = v.vType.vType, colorValue = color) +
              createCell(cellData = v.vType.size.toString, colorValue = color) +
              createCell(cellData = v.bAddr.toHexString, colorValue = color) +
              createCell(cellData = boxesForVertex, colorValue = color) +
              rowEnd +
              rows +
              rowStart +
              createCell(cellData = "memory region end", portValue = portHead, colorValue = color, colSpanValue = "3") +
              createCell(cellData = v.eAddr.toHexString, colorValue = color) +
              createCell(cellData = "", colorValue = color) +
              rowEnd + (if (surEPTs != "") {
                surEPTs
              } else { "" })

          case v: DsOliVertexNull =>
            rowStart +
              createCell(cellData = "NULL", portValue = portHead, colorValue = colorValPredfinedVertex, colSpanValue = "5") +
              rowEnd
          case v: DsOliVertexUndef =>
            rowStart +
              createCell(cellData = "UNDEF", portValue = portHead, colorValue = colorValPredfinedVertex, colSpanValue = "5") +
              rowEnd
          case _ =>
            rowStart +
              createCell(cellData = innerNode.value.id.toString, portValue = portHead, colorValue = colorValPredfinedVertex, colSpanValue = "4") +
              rowEnd

        }) + tableEnd + ">"

    return table
  }

  /**
   * Create the DOT for a connected node
   *
   * @param innerNode the vertex to transform
   * @return Option the DOT graph and the DOT node statement
   */
  def cNodeTransformer(innerNode: scalax.collection.Graph[DsOliVertex, DsOliDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] = {
    Some(
      DotSubGraph(
        ancestor = dotHeader,
        subgraphId = "connected_vertices",
        kvList = Seq(DotAttr("rankdir", "LR"))),
      DotNodeStmt(innerNode.value.id.toString, Seq(
        DotAttr("label", nodeToHTML(innerNode)),
        DotAttr("shape", "none"))))
  }

  /**
   * Create the DOT for a disconnected node
   *
   * @param innerNode the vertex to transform
   * @return Option the DOT graph and the DOT node statement
   */
  def iNodeTransformer(innerNode: scalax.collection.Graph[DsOliVertex, DsOliDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] = {
    Some(
      DotSubGraph(
        ancestor = dotHeader,
        subgraphId = "disconnected_vertices",
        kvList = Seq.empty),
      DotNodeStmt(innerNode.value.id.toString, Seq(
        DotAttr("label", nodeToHTML(innerNode)),
        DotAttr("shape", "none"))))
  }

  /**
   * Main entry function to create the DOT graph output
   *
   * @param graph the graph to print
   * @param boxes Option the strands to print
   * @param epts the entry pointer tags to print
   * @param i the main time step
   * @param u the artificial time step
   * @param isArtificial flag to indicate artificial event
   * @return a string representation of the graph
   */
  def createGraphOutput(graph: DsOliGraph, boxes: Option[DsOliBoxStep], epts: DsOliFeatureSet, i: Int, u: Int, isArtificial: Boolean): String = {
    val funSignature = classSignature + "createGraphOutput: "

    // Class operates on global variables
    currentGraph = graph
    lastEdge = currentGraph.lastEdge
    this.boxesOpt = boxes
    this.epts = epts
    this.i = i
    this.u = u
    this.isArtificial = isArtificial

    try {
      graph.graph.toDot(dotHeader, edgeTransformer, cNodeTransformer = Some(cNodeTransformer), iNodeTransformer = Some(iNodeTransformer))
    } catch {
      case e: Exception =>
        DsOliLogger.error(funSignature + "i = " + i + ", u = " + u + ", isArtificial = " + isArtificial)
        throw e
    }
  }
}