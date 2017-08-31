
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
 * DSITypesCreator.scala created on Aug 30, 2016
 *
 * Description: Type refinement for Howard types
 */
package binarytypes

import pointstograph.DsOliGraph
import scala.collection.mutable.ListBuffer
import pointstograph.DsOliDiEdge
import pointstograph.DsOliVertex
import pointstograph.DsOliVertexMemory
import extlogger.DsOliLogger
import pointstograph.DsOliVertexField
import pointstograph.FieldType._
import scala.util.control.Breaks._
import pointstograph.DsOliTypeVertex
import pointstograph.DsOliVertex
import pointstograph.DsOliVertexMemory
import pointstograph.DsOliTypeVertex
import scala.collection.mutable.HashMap
import pointstograph.DsOliTypeVertex
import pointstograph.DsOliTypeVertex
import pointstograph.DsOliTypeVertex
import pointstograph.DsOliType
import pointstograph.ITypeDB
import pointstograph.DsOliVertexField
import pointstograph.DsOliTypeVertex
import pointstograph.DsOliTypeVertex
import pointstograph.DsOliVertexField
import pointstograph.DsOliVertex
import pointstograph.DsOliTypeVertex
import java.io.PrintWriter
import java.io.File
import util.DsOliPathUtils
import pointstograph.DsOliTypeVertex
import java.io.FileWriter
import scala.io.Source
import java.io.StringWriter
import java.io.Writer
import pointstograph.DsOliTypeVertex

/**
 * @author DSI
 *
 */
class DSITypesCreator(typeDB: ITypeDB) {
  val classSignature = "DSITypesCreator::"

  /**
   * Calculate the offset of pointer fields relative from
   * the incoming edge.
   *
   * @param fields the fields of the vertex
   * @param edge the incoming edge
   * @return list with tuple (number of pointer field, pointer offset)
   */
  def calculatePointerOffsets(fields: ListBuffer[DsOliVertexField], edge: DsOliDiEdge[DsOliVertex]): ListBuffer[(Long, Long)] = {
    val offsets = new ListBuffer[(Long, Long)]
    for (i <- 0 until fields.length) {
      if (fields(i).fType == Pointer || fields(i).cType.contains("INT64"))
        offsets.append((i, fields(i).vOffset - edge.tOffset))
    }
    return offsets
  }

  /**
   * Maps between the memory layout offset to the index in the fields array
   *
   * @param fields the fields of the vertex
   * @param offset the offset to map
   * @return Option the index corresponding to the offset
   */
  def getFieldIndexForOffset(fields: ListBuffer[DsOliVertexField], offset: Long): Option[Int] = {
    val funSignature = classSignature + "getFieldIndexForOffset: "
    DsOliLogger.debug(funSignature + " entered: offset: " + offset + " fields: " + fields)
    for (i <- 0 until fields.length) {
      val field = fields(i)
      if (field.vOffset <= offset && offset < field.vOffset + field.vSize)
        return Some(i)
    }
    None
  }

  /**
   * Fetch longest DSI types
   *
   * @param dsiTypes list of tuple (DSI type, DSI type length)
   * @return list of the longest DSI types
   */
  def maxLengthDSITypes(dsiTypes: ListBuffer[(DSIType, Long)]): ListBuffer[(DSIType, Long)] = {
    val funSignature = classSignature + "maxLengthDSITypes: "
    val maxLength = dsiTypes.foldLeft(0)((max, curItem) => Math.max(max, curItem._1.size.toInt))
    DsOliLogger.debug(funSignature + " maxLength: " + maxLength)
    dsiTypes.filter(_._1.size == maxLength)
  }

  /**
   * Compare two vertex fields if they are equal
   *
   * @param first the first field to compare
   * @param second the second field to compare
   * @return Boolean true if equal
   */
  def comparePrimitiveTypes(first: DsOliVertexField, second: DsOliVertexField): Boolean = {
    first == second ||
      // Check for a don't care type -> untyped memory region
      (first.cType.contains("X") || second.cType.contains("X")) ||
      // Either source and or target can contain INT64, which is also considered a pointer at the moment
      (first.cType.contains("INT64") || first.cType.contains("VOID *")) &&
      (second.cType.contains("INT64") || second.cType.contains("VOID *"))
  }

  /**
   * Compare two DSI types, by checking all their fields
   * for primitive type compatibility.
   *
   * @param first the first DSI Type
   * @param second the second DSI Type
   * @return Boolean true if equal
   */
  def compareAllPrimitiveTypes(first: DSIType, second: DSIType): Boolean = {
    val funSignature = classSignature + "compareAllPrimtiveTypes: "

    if (first.size != second.size) return false

    for (i <- 0 until first.size.toInt) {
      if (!comparePrimitiveTypes(first.primitiveTypes(i), second.primitiveTypes(i))) {
        DsOliLogger.debug(funSignature + "failed! missmatch: " + first.primitiveTypes(i) + " != " +
          second.primitiveTypes(i))
        return false
      }
    }
    return true
  }

  /**
   * Flatten a DSI type, i.e., remove all nestings
   *
   * @param dsType the DSI type
   * @param globalOffset the offset starting from the beginning of the type
   * @return a list of primitive type fields
   */
  def getPrimitiveTypesFlattenedRec(dsType: DsOliType, globalOffset: Int): ListBuffer[DsOliVertexField] = {
    val funSignature = classSignature + "getPrimitiveTypesFlattenedRec: "
    DsOliLogger.debug(funSignature + "entered: globalOffset: " + globalOffset)
    DsOliLogger.debug(funSignature + "\tdsType: " + dsType)
    val primitiveTypes = new ListBuffer[DsOliVertexField]
    var localOffset = 0

    // Safety check
    if (dsType.fields == null) return primitiveTypes

    dsType.fields.foreach {
      field =>
        // Always check for an Undefined memory region (i.e. not typed)
        DsOliLogger.debug(funSignature + "comparing offsets (field.vOffset) != localOffset: " + (field.vOffset) + " != " + localOffset)
        if (field.vOffset != localOffset) {
          if (field.vOffset - localOffset < 0) throw new Exception("Negative size calculated: field: " + field + "\nlocalOffset: " + localOffset)
          val newField = new DsOliVertexField(
            "undef_region",
            "X",
            field.vOffset - localOffset,
            globalOffset + localOffset,
            Undefined)
          DsOliLogger.debug(funSignature + "adding untyped newField: " + newField)
          primitiveTypes.append(newField)
        }
        localOffset = field.vOffset + field.vSize
        DsOliLogger.debug(funSignature + "new localOffset: " + localOffset)
        if (field.fType == Compound) {
          val compoundType = typeDB.getTypeObject(field.cType).get
          DsOliLogger.debug(funSignature + "recurse on field: " + field)
          val recPrimitiveTypes = getPrimitiveTypesFlattenedRec(compoundType, globalOffset + field.vOffset)
          DsOliLogger.debug(funSignature + "adding fields after recurse: " + recPrimitiveTypes)
          primitiveTypes.appendAll(recPrimitiveTypes)
          DsOliLogger.debug(funSignature + "fields after adding recurse: " + primitiveTypes)
        } else {
          val newField = new DsOliVertexField(
            field.name,
            field.cType,
            field.vSize,
            globalOffset + field.vOffset,
            field.fType)
          DsOliLogger.debug(funSignature + "adding newField: " + newField)
          primitiveTypes.append(newField)
        }
    }

    DsOliLogger.debug(funSignature + "comparing offsets dsType.size != localOffset: " + dsType.size + " != " + localOffset)
    if (localOffset != dsType.size) {
      val newField = new DsOliVertexField(
        "undef_region",
        "X",
        dsType.size - localOffset,
        localOffset,
        Undefined)
      DsOliLogger.debug(funSignature + "adding untyped at the end newField: " + newField)
      primitiveTypes.append(newField)
    }
    DsOliLogger.debug(funSignature + "done, returing: " + primitiveTypes)
    return primitiveTypes
  }

  /**
   * Create possible slices/sub types of a DSI type which have more
   * than one element.
   *
   * @param dsiTypes list of DSI types with linkage offsets
   * @return a list of all created sub types
   */
  def createAllPossibleSubTypeChunks(dsiTypes: ListBuffer[(DSIType, Long)]): ListBuffer[(DSIType, Long)] = {
    val funSignature = classSignature + "createAllPossibleSubTypeChunks: "
    DsOliLogger.debug(funSignature + "entered:")
    val allSubTypeChunks = new ListBuffer[(DSIType, Long)]
    dsiTypes.foreach {
      dsiTypeTuple =>
        val (dsiType, offset) = dsiTypeTuple
        DsOliLogger.debug(funSignature + "inspecting: dsiType (groupId: " + dsiType.groupId + ", linkageOffset: " + dsiType.linkageOffset +
          ", offset in soruce: " + offset + ": " + dsiType.primitiveTypes)

        // First get the index for the linkage offset, as we are interested in all elements past this
        // offset (which in fact are past the index)
        // ! Important ! The linkageOffset needs the offset from the start of the type
        DsOliLogger.debug(funSignature + "linkageOffset: " + dsiType.linkageOffset + ", vOffset: " + dsiType.primitiveTypes.head.vOffset)
        val index = getFieldIndexForOffset(dsiType.primitiveTypes, dsiType.linkageOffset + dsiType.primitiveTypes.head.vOffset).get
        DsOliLogger.debug(funSignature + "index: " + index)

        // Now iterate over all primitive types starting from the element past the current index
        // and create one type hypothesis for each slice
        for (i <- index + 1 until dsiType.primitiveTypes.length) {

          DsOliLogger.debug(funSignature + "iterating: " + i)
          // Fetch the slice
          val primitiveTypeSlice = dsiType.primitiveTypes.slice(0, i)
          DsOliLogger.debug(funSignature + "primitiveTypeSlice: " + primitiveTypeSlice)

          // Only consider slices, which have more than one element!
          if (primitiveTypeSlice.size > 1) {
            // Create a new type, which contains the type slice, but still has the same linkage offset
            val newDsiType = new DSIType(primitiveTypeSlice, dsiType.linkageOffset)
            // ! Important ! mark that this newly created type is in the same group
            newDsiType.groupId = dsiType.groupId
            DsOliLogger.debug(funSignature + "created: newDsiType (id: " + newDsiType.id + " linkageOffset: " + newDsiType.linkageOffset + ", offset in soruce: " + offset
              + ": " + newDsiType.primitiveTypes)

            allSubTypeChunks.append((newDsiType, offset))
          }

        }
    }
    dsiTypes.appendAll(allSubTypeChunks)
    DsOliLogger.debug(funSignature + "all created sub types:")
    dsiTypes.foreach {
      dsiTypeTuple =>
        val (dsiType, offset) = dsiTypeTuple
        DsOliLogger.debug(funSignature + "found: dsiType (id: " + dsiType.id + " groupId: " + dsiType.groupId + ", linkageOffset: " + dsiType.linkageOffset +
          ", offset in soruce: " + offset + ": " + dsiType.primitiveTypes)
    }
    dsiTypes
  }

  /**
   * Calculate the possible DSI types for a pointer connection between source and
   * target type vertices.
   *
   * @param edge the edge connecting the type vertices
   * @return a list of possible DSI types with their linkage offset
   */
  def calculateDSITypesForEdge(edge: DsOliDiEdge[DsOliVertex]): ListBuffer[(DSIType, Long)] = {
    val funSignature = classSignature + "calculateDSITypesForEdge: "
    val dsiTypesWithPosition = new ListBuffer[(DSIType, Long)]

    if (edge.source.isInstanceOf[DsOliVertexMemory] && edge.target.isInstanceOf[DsOliVertexMemory]) {

      val source = edge.source.asInstanceOf[DsOliVertexMemory]
      val target = edge.target.asInstanceOf[DsOliVertexMemory]

      if (source.vType.fields != null && target.vType.fields != null) {
        // IMPORTANT: we have two different offsets:
        // a) byte offset from the beginning of the memory chunk: always referred to as offset
        // b) index / offset within the primitive type array of the vertex: always referred to as index
        DsOliLogger.debug(funSignature + " processing edge (" + edge.source.id + ":" +
          edge.sOffset + " -> " + edge.target.id + ":" + edge.tOffset + "): " + edge)

        // Flatten the source type
        DsOliLogger.debug(funSignature + "flatten source: " + source.vType.fields)
        val sourceTypeSeq = getPrimitiveTypesFlattenedRec(source.vType, 0)

        // Flatten the target type
        DsOliLogger.debug(funSignature + "flatten target: " + target.vType.fields)
        val targetTypeSeqFlattened = getPrimitiveTypesFlattenedRec(target.vType, 0)
        // Get the flattened target slice starting at the incoming edge 
        val indexTargetOffset = getFieldIndexForOffset(targetTypeSeqFlattened, edge.tOffset.toInt).get
        val targetTypeSeq = targetTypeSeqFlattened.slice(indexTargetOffset, targetTypeSeqFlattened.length)

        DsOliLogger.debug(funSignature + "flattened source seq: " + sourceTypeSeq)
        DsOliLogger.debug(funSignature + "flattened target seq: " + targetTypeSeq)

        // Find the pointer offsets in the target, as only pointers are of interest
        val indexAndLinkageOffsetsInTargetTuple = calculatePointerOffsets(targetTypeSeq, edge)

        // At the moment assume, that we always find the corresponding field
        val indexSourceOffset = getFieldIndexForOffset(sourceTypeSeq, edge.sOffset).get

        DsOliLogger.debug(funSignature + " index and linkage offsets in target: " + indexAndLinkageOffsetsInTargetTuple)
        DsOliLogger.debug(funSignature + " index of source offset (" + edge.sOffset + "): " + indexSourceOffset)

        // Cycle through the linkage offsets and calculate the
        // possible mapping between source and target for each
        // linkage offset. Target sequence is between incoming
        // pointer and end of target; Obviously, target sequence 
        // can be mapped into source only within the boundaries
        // of the source.
        val maxEnd = Math.max(0, indexSourceOffset - targetTypeSeq.length)
        indexAndLinkageOffsetsInTargetTuple.foreach {
          offsets =>
            val (indexOffset, linkageOffset) = offsets
            // Need to calculate the byte offset for the mapping
            val sourceStartOffset = edge.sOffset - linkageOffset
            if (sourceStartOffset >= 0) {

              DsOliLogger.debug(funSignature + " testing linkage offset: " + indexOffset + " linkageOffset: " +
                linkageOffset + " sourceStartOffset: " + sourceStartOffset)

              val foundDSIType = new ListBuffer[DsOliVertexField]
              breakable {

                // Step through each field of the target type sequence
                for (u <- 0 until targetTypeSeq.length) {
                  val sourceCurOffset = sourceStartOffset + (targetTypeSeq(u).vOffset - edge.tOffset)

                  DsOliLogger.debug(funSignature + "sourceCurOffset: " + sourceCurOffset + " targetTypeSeq(u).vOffset: " +
                    targetTypeSeq(u).vOffset + " edge.tOffset: " + edge.tOffset)
                  DsOliLogger.debug(funSignature + "sourceStartOffset: " + sourceStartOffset)

                  // Check, if we have reached the end of the source sequence
                  val sourceCurIndexOpt = getFieldIndexForOffset(sourceTypeSeq, sourceCurOffset)
                  if (sourceCurIndexOpt.isEmpty) {
                    DsOliLogger.debug(funSignature + "reached end of source, stopping -> no match for sourceCurOffset: " + sourceCurOffset)
                    break
                  }
                  val sourceCurIndex = sourceCurIndexOpt.get
                  DsOliLogger.debug(funSignature + "testing target: " + targetTypeSeq(u) + " source: " + sourceTypeSeq(sourceCurIndex))

                  // Source and target need to be compatible on the primitive types
                  if (comparePrimitiveTypes(targetTypeSeq(u), sourceTypeSeq(sourceCurIndex))) {

                    DsOliLogger.debug(funSignature + "creating new field with linkage offset: " + (targetTypeSeq(u).vOffset - edge.tOffset.toInt))
                    DsOliLogger.debug(funSignature + "targetTypeSeq(u).vOffset: " + (targetTypeSeq(u).vOffset))
                    DsOliLogger.debug(funSignature + "edge.tOffset.toInt: " + (edge.tOffset.toInt))

                    // Record the primitive type
                    val newField = new DsOliVertexField(targetTypeSeq(u).name,
                      targetTypeSeq(u)cType,
                      targetTypeSeq(u).vSize,
                      targetTypeSeq(u).vOffset - edge.tOffset.toInt,
                      targetTypeSeq(u).fType)
                    foundDSIType.append(newField)
                    DsOliLogger.debug(funSignature + "matched")
                  } else {
                    DsOliLogger.debug(funSignature + "No match, exiting")
                    break
                  }
                }
              }

              // Skip empty type && types, which do not include the linkage edge
              if (foundDSIType.length > 1 && getFieldIndexForOffset(foundDSIType, linkageOffset).isDefined) {
                DsOliLogger.debug(funSignature + "Found DSI type: " + foundDSIType)

                val newDsiType = new DSIType(foundDSIType, linkageOffset)
                // Test, if another edge has already produced the correct type before
                if (!source.asInstanceOf[DsOliTypeVertex].dsiTypeInstances.exists {
                  typeInstancesTyple =>
                    val (testDsiType, testOffset) = typeInstancesTyple
                    val primitiveTypesEqual = compareAllPrimitiveTypes(testDsiType, newDsiType)
                    val startOffsetsEqual = testOffset == sourceStartOffset
                    val linkageOffsetsEqual = testDsiType.linkageOffset == linkageOffset
                    // Ignore linkage offset
                    //primitiveTypesEqual && startOffsetsEqual 
                    // Take linkage offset into account
                    primitiveTypesEqual && startOffsetsEqual && linkageOffsetsEqual
                }) {
                  DsOliLogger.debug(funSignature + "created: newDsiType (id: " + newDsiType.id + " linkageOffset: " +
                    newDsiType.linkageOffset + ", offset in soruce: skipped : " + newDsiType.primitiveTypes)
                  dsiTypesWithPosition.append((newDsiType, sourceStartOffset))
                } else {
                  DsOliLogger.debug(funSignature + "skipping newDsiType because it already exists at source (id: " +
                    newDsiType.id + " linkageOffset: " + newDsiType.linkageOffset + ", offset in soruce: skipped : " +
                    newDsiType.primitiveTypes)
                }
              }
            } else {
              DsOliLogger.debug(funSignature + " sourceStartOffset < 0 -> Skip: " + sourceStartOffset +
                " testing linkage offset: " + indexOffset + " linkageOffset: " + linkageOffset)
            }
        }
      }
    }
    DsOliLogger.debug(funSignature + "all found dsi types: ")
    dsiTypesWithPosition.foreach {
      typeTuple =>
        val (dsiType, offset) = typeTuple
        DsOliLogger.debug(funSignature + "dsiType (@vertex offset: " + offset + "): " + dsiType)
    }

    createAllPossibleSubTypeChunks(dsiTypesWithPosition)
  }

  /**
   * Find all edges in the merged type graph, which start at
   * the given DSI type and vertex obeying the linkage offset
   *
   * @param dsiType the DSI type
   * @param offsetInVertex the offset of the type within the vertex
   * @param vertex the vertex where the edges originate from
   * @param mergedTypeGraph the merged type graph
   * @return a list of outgoing edges
   */
  def getOutgoingEdgesFromType(dsiType: DSIType, offsetInVertex: Long, vertex: DsOliTypeVertex, mergedTypeGraph: DsOliGraph): ListBuffer[DsOliDiEdge[DsOliVertex]] = {
    val funSignature = classSignature + "getOutgoingEdgesFromType: "

    // Iterate through all edges of the merged type graph
    //mergedTypeGraph.graph.edges.iterator.filter {
    mergedTypeGraph.graph.edges.toList.sortBy(edge => edge.toOuter.id).iterator.filter {
      edgeInner =>
        val edge = edgeInner.toOuter
        DsOliLogger.debug(funSignature + " offsetInVertex: " + offsetInVertex + " edge.sOffset: " + edge.sOffset)
        DsOliLogger.debug(funSignature + " linkageOffset: " + dsiType.linkageOffset)
        DsOliLogger.debug(funSignature + " edge.source == vertex: " + (edge.source == vertex) +
          " edge.sOffset - offsetInVertex == dsiType.linkageOffset: " + (edge.sOffset - offsetInVertex == dsiType.linkageOffset))
        // Edge source must be the vertex and the offset within the DSI type should
        // be  the linkage offset
        edge.source == vertex &&
          edge.sOffset - offsetInVertex == dsiType.linkageOffset
    }.foldLeft(new ListBuffer[DsOliDiEdge[DsOliVertex]]) { (list, item) => list.append(item.toOuter); list }

  }

  /**
   *  Find all incoming edges to the start of a DSI type
   *
   *  @param dsiType the DSI type
   *  @param offsetInVertex the offset of the DSI type in the vertex
   *  @param vertex the target vertex where the DSI type resides
   *  @param mergedTypeGraph the merged type graph
   *  @return list of incoming edges
   */
  def getIncomingEdgesForType(dsiType: DSIType, offsetInVertex: Long, vertex: DsOliTypeVertex, mergedTypeGraph: DsOliGraph): ListBuffer[DsOliDiEdge[DsOliVertex]] = {
    val funSignature = classSignature + "getIncomingEdgesForType: "
    //mergedTypeGraph.graph.edges.iterator.filter {
    mergedTypeGraph.graph.edges.toList.sortBy(edge => edge.toOuter.id).iterator.filter {
      edgeInner =>
        val edge = edgeInner.toOuter
        DsOliLogger.debug(funSignature + " offsetInVertex: " + offsetInVertex + " edge.tOffset: " + edge.tOffset)
        DsOliLogger.debug(funSignature + " edge.target == vertex: " + (edge.target == vertex) + " edge.tOffset == offsetInVertex: " + (edge.tOffset == offsetInVertex))
        // Target must be vertex and incoming address must be start of DSI type
        edge.target == vertex &&
          edge.tOffset == offsetInVertex
    }.foldLeft(new ListBuffer[DsOliDiEdge[DsOliVertex]]) { (list, item) => list.append(item.toOuter); list }
  }

  /**
   * Checks, if the DSI type can be propagated along the edge onto the
   * target obeying primitive types.
   *
   * @param dsiType the DSI type to propagate
   * @param edge the edge to propagate on
   * @return Boolean
   */
  def typeCanBePropagatedToTarget(dsiType: DSIType, edge: DsOliDiEdge[DsOliVertex]): Boolean = {
    val funSignature = classSignature + "typeCanBePropagatedToTarget: "
    DsOliLogger.debug(funSignature + "entered: " + dsiType)

    // Fetch the target
    val target = edge.target.asInstanceOf[DsOliTypeVertex]

    // If the target does not contain any fields, stop immediately
    if (target.vType.fields == null) return false

    // Flatten the target
    val targetTypeSeqFlattened = getPrimitiveTypesFlattenedRec(target.vType, 0)
    val targetIndex = getFieldIndexForOffset(targetTypeSeqFlattened, edge.tOffset).get
    for (i <- 0 until dsiType.size.toInt) {
      val targetOffset = edge.tOffset + dsiType.primitiveTypes(i).vOffset
      val targetCompareIndexOpt = getFieldIndexForOffset(targetTypeSeqFlattened, targetOffset)
      if (targetCompareIndexOpt.isEmpty) {
        DsOliLogger.debug(funSignature + "failed! no corresponding element for offset: " + targetOffset)
        return false
      }
      // Are the fields compatible regarding the primitive types
      if (!comparePrimitiveTypes(dsiType.primitiveTypes(i), targetTypeSeqFlattened(targetCompareIndexOpt.get))) {
        DsOliLogger.debug(funSignature + "failed! missmatch: " + dsiType.primitiveTypes(i) + " != " +
          targetTypeSeqFlattened(targetCompareIndexOpt.get))
        return false
      }
    }
    DsOliLogger.debug(funSignature + " passed! all types are matching in the target")
    return true
  }

  /**
   * Checks, if the DSI type can be propagated along the edge onto the
   * source obeying primitive types.
   *
   * @param dsiType the DSI type to propagate
   * @param edge the edge to propagate on
   * @return Option the offset on which to propagate
   */
  def typeCanBePropagatedToSource(dsiType: DSIType, edge: DsOliDiEdge[DsOliVertex]): Option[Long] = {
    val funSignature = classSignature + "typeCanBePropagatedToSource: "
    DsOliLogger.debug(funSignature + "entered: " + dsiType)
    val dsiTypes = calculateDSITypesForEdge(edge)
    DsOliLogger.debug(funSignature + " processing dsiTypes" + dsiTypes)
    dsiTypes.foreach {
      dsiTypeTuple =>
        val (testDSIType, offset) = dsiTypeTuple
        DsOliLogger.debug(funSignature + " processing test type(" + offset + "): " + testDSIType)
        DsOliLogger.debug(funSignature + " dsiType.linkageOffset == testDSIType.linkageOffset " + dsiType.linkageOffset + " == " + testDSIType.linkageOffset)
        if (compareAllPrimitiveTypes(dsiType, testDSIType) &&
          dsiType.linkageOffset == testDSIType.linkageOffset &&
          !edge.source.asInstanceOf[DsOliTypeVertex].dsiTypeInstances.exists {
            instanceTuple =>
              val (instanceType, instanceOffset) = instanceTuple
              val typesEqual = instanceType.id == dsiType.id
              DsOliLogger.debug(funSignature + " testDSIType(offset: " + offset + "): " + testDSIType)
              DsOliLogger.debug(funSignature + " instanceType(offset: " + instanceOffset + "): " + instanceType)
              val isOverlappingTest = isOverlapping(testDSIType, offset, instanceType, instanceOffset) && (offset != instanceOffset)
              DsOliLogger.debug(funSignature + " typesEqual: " + typesEqual + " isOverlappingTest: " + isOverlappingTest)
              typesEqual && isOverlappingTest
          }) {
          DsOliLogger.debug(funSignature + " found type (linkageOffset: " + testDSIType.linkageOffset + "):" + dsiType)
          return Some(offset)
        }
    }
    DsOliLogger.debug(funSignature + " type not found in source")
    None
  }

  /**
   * Recursively propagate a DSI type along the edges
   *
   * @param dsiType the type to propagate
   * @param offsetInVertex the offset of the type in the vertex
   * @param sourceTypeVertex the source vertex
   * @param mergedTypeGraph the merged type graph
   *
   */
  def propagateDSITypeOnMaximalPath(dsiType: DSIType, offsetInVertex: Long, sourceTypeVertex: DsOliVertex, mergedTypeGraph: DsOliGraph): Unit = {
    val funSignature = classSignature + "propagateDSITypeOnMaximalPath: "
    DsOliLogger.debug(funSignature + "entered: dsiType: " + dsiType + " offsetInVertex: " + offsetInVertex + " sourceTypeVertex: " + sourceTypeVertex)
    if (sourceTypeVertex.isInstanceOf[DsOliTypeVertex]) {
      val source = sourceTypeVertex.asInstanceOf[DsOliTypeVertex]
      // Bread crumb check
      if (!source.dsiTypeInstances.exists(typeInstance => typeInstance._1 == dsiType && typeInstance._2 == offsetInVertex)) {
        source.dsiTypeInstances.append((dsiType, offsetInVertex))

        // Fetch incoming and outgoing edges to/from the type
        val outEdgesFromType = getOutgoingEdgesFromType(dsiType, offsetInVertex, source, mergedTypeGraph)
        val inEdgesToType = getIncomingEdgesForType(dsiType, offsetInVertex, source, mergedTypeGraph)

        // Process all outgoing edges
        outEdgesFromType.foreach {
          edge =>
            DsOliLogger.debug(funSignature + "Outgoing edge: " + edge.source.id + ":" + edge.sOffset + " -> " + edge.target.id + ":" + edge.tOffset)
            if (typeCanBePropagatedToTarget(dsiType, edge)) {
              DsOliLogger.debug(funSignature + "Outgoing edge can be used to propagate type, recurse.")
              propagateDSITypeOnMaximalPath(dsiType, edge.tOffset, edge.target, mergedTypeGraph)
            } else {
              DsOliLogger.debug(funSignature + "Outgoing can not be used to propagate type, stop.")
            }
        }

        // Process all incoming edges
        inEdgesToType.foreach {
          edge =>
            DsOliLogger.debug(funSignature + "Incoming edge: " + edge.source.id + ":" + edge.sOffset + " -> " + edge.target.id + ":" + edge.tOffset)
            val sourceOffsetOpt = typeCanBePropagatedToSource(dsiType, edge)
            if (sourceOffsetOpt.isDefined) {
              DsOliLogger.debug(funSignature + "Incoming edge can be used to propagate type, recurse.")
              propagateDSITypeOnMaximalPath(dsiType, sourceOffsetOpt.get, edge.source, mergedTypeGraph)
            } else {
              DsOliLogger.debug(funSignature + "Incoming can not be used to propagate type, stop.")
            }
        }
      } else {
        DsOliLogger.debug(funSignature + " there already exists an instance of the type at this offset in the vertex. Done.")
      }
    } else {
      DsOliLogger.debug(funSignature + "source not of type DsOliTypeVertex!")
    }
  }

  /**
   * Collect all instances of each DSI type inside of a type vertex
   *
   * @param vertex the type vertex
   * @return a tuple list with the DSI type and a list of offsets where the type starts
   */
  def getAllInstancesPerTypes(vertex: DsOliTypeVertex): List[(DSIType, ListBuffer[Long])] = {
    val funSignature = classSignature + "getAllInstancesPerTypes: "
    DsOliLogger.debug(funSignature + " entered: vertex: " + vertex)
    val instancesPerType = new HashMap[DSIType, ListBuffer[Long]]()
    // Cycle through DSI type instances
    vertex.dsiTypeInstances.foreach {
      dsiTypeInstance =>
        val (dsiType, offset) = dsiTypeInstance
        DsOliLogger.debug(funSignature + " dsiType (@off: " + offset + "): " + dsiType)
        // Group instances by type and record offset
        if (!instancesPerType.contains(dsiType)) {
          instancesPerType.put(dsiType, new ListBuffer[Long])
        }
        instancesPerType.get(dsiType).get.append(offset)
    }
    return instancesPerType.toList
  }

  /**
   * Calculate combinations between all list elements
   *
   * @param list the list to produce combinations
   * @return a list of combination tuples
   */
  def combinations[A](list: List[A]): ListBuffer[(A, A)] = {
    val listCombinations = new ListBuffer[(A, A)]
    for (i <- 0 until list.length) {
      // u <- i: include self combination; u <- i+1: exclude self combination
      for (u <- i until list.length) {
        listCombinations.append((list(i), list(u)))
      }
    }
    return listCombinations
  }

  /**
   * Checks, if two DSI types are nested
   *
   * @param firstType the first DSI type
   * @param firstOffset the offset of the first DSI type
   * @param secondType the second DSI type
   * @param secondOffset the offset of the second DSI type
   * @return Boolean
   */
  def isNested(firstType: DSIType, firstOffset: Long,
    secondType: DSIType, secondOffset: Long): Boolean = {
    val funSignature = classSignature + "isNested: "
    DsOliLogger.debug(funSignature + "entered")
    DsOliLogger.debug(funSignature + "firstType.primitiveTypes: " + firstType.primitiveTypes + " firstType.byteSize: " + firstType.byteSize)
    DsOliLogger.debug(funSignature + "secondType.primitiveTypes: " + secondType.primitiveTypes + " secondType.byteSize: " + secondType.byteSize)

    val firstNestedInsideSecond = (firstOffset >= secondOffset && (firstOffset + firstType.byteSize <= secondOffset + secondType.byteSize))
    val secondNestedInsideFirst = (secondOffset >= firstOffset && (secondOffset + secondType.byteSize <= firstOffset + firstType.byteSize))

    firstNestedInsideSecond || secondNestedInsideFirst
  }

  /**
   * Checks, if two DSI types are overlapping
   *
   * @param firstType the first DSI type
   * @param firstOffset the offset of the first DSI type
   * @param secondType the second DSI type
   * @param secondOffset the offset of the second DSI type
   * @return Boolean
   */
  def isOverlapping(firstType: DSIType, firstOffset: Long,
    secondType: DSIType, secondOffset: Long): Boolean = {
    val funSignature = classSignature + "isOverlapping: "
    DsOliLogger.debug(funSignature + "entered")
    DsOliLogger.debug(funSignature + "firstType.primitiveTypes: " + firstType.primitiveTypes + " firstType.byteSize: " + firstType.byteSize)
    DsOliLogger.debug(funSignature + "secondType.primitiveTypes: " + secondType.primitiveTypes + " secondType.byteSize: " + secondType.byteSize)
    DsOliLogger.debug(funSignature + "firstOffset: " + firstOffset + " ,  secondOffset: " + secondOffset)
    val overlapFirstInSecond = (firstOffset > secondOffset && firstOffset < secondOffset + secondType.byteSize)
    DsOliLogger.debug(funSignature + "secondType.primitiveTypes: " + secondType.primitiveTypes + " secondType.byteSize: " + secondType.byteSize)
    val overlapSecondInFirst = (secondOffset > firstOffset && secondOffset < firstOffset + firstType.byteSize)
    overlapFirstInSecond || overlapSecondInFirst
  }

  /**
   * Checks, if two DSI types are compatible
   *
   * @param firstType the first DSI type
   * @param firstOffset the offset of the first DSI type
   * @param secondType the second DSI type
   * @param secondOffset the offset of the second DSI type
   * @return Boolean
   */
  def isCompatible(firstType: DSIType, firstOffset: Long,
    secondType: DSIType, secondOffset: Long): Boolean = {
    val funSignature = classSignature + "isCompatible: "
    DsOliLogger.debug(funSignature + "entered: first groupId: " + firstType.groupId + ", second groupId: " + secondType.groupId)

    // Types from the same group can never by grouped together!
    // The only exception is, that first and second type can of course be the same type, which means, 
    // that one needs to check for type equivalence first!
    if (firstType.id != secondType.id && firstType.groupId == secondType.groupId) {
      DsOliLogger.debug(funSignature + "pre check: group ids failed => groups equal: firstType.id: " + firstType.id + ", secondType.id: " + secondType.id)
      return false
    }
    // Types equal:
    // Offset identical -> true
    if (firstType == secondType && firstOffset == secondOffset) {
      DsOliLogger.debug(funSignature + "first")
      return true
    }
    // Offset not identical -> nesting test & overlap test
    if (firstType == secondType && firstOffset != secondOffset) {
      DsOliLogger.debug(funSignature + "second")
      // Same type is not allowed to overlap and not allowed to be nested
      return (!isOverlapping(firstType, firstOffset, secondType, secondOffset) &&
        !isNested(firstType, firstOffset, secondType, secondOffset))
    }
    // Types different:
    // Offset identical -> test on type equivalence -> false
    if (firstType != secondType && firstOffset == secondOffset && compareAllPrimitiveTypes(firstType, secondType)) {
      DsOliLogger.debug(funSignature + "third")
      return false
    }
    // Offset identical -> no type equivalence -> should always be nesting!?
    if (firstType != secondType && firstOffset == secondOffset) {
      DsOliLogger.debug(funSignature + "fourth")
      return isNested(firstType, firstOffset, secondType, secondOffset)
    }
    // Offset different -> do overlap test and nesting test
    if (firstType != secondType && firstOffset != secondOffset) {
      DsOliLogger.debug(funSignature + "fifth")
      // Different types are not allowed to overlap but might be nested (needs no explicit test)
      return !isOverlapping(firstType, firstOffset, secondType, secondOffset)
    }

    DsOliLogger.debug(funSignature + "default")
    false
  }

  /**
   * Do the first local compatibility check, which is within one
   * type vertex. The resulting type compatibility is stored within
   * each type vertex of the merged type graph.
   *
   * @param mergedTypeGraph the merged type graph
   */
  def compatibleTypesStage1(mergedTypeGraph: DsOliGraph): Unit = {
    val funSignature = classSignature + "compatibleTypesStage1: "
    DsOliLogger.debug(funSignature + "entered")

    // Cycle through all nodes
    mergedTypeGraph.graph.nodes.iterator.foreach {
      nodeInner =>
        // Fetch all DSI type instances
        val vertex = nodeInner.value.asInstanceOf[DsOliTypeVertex]
        DsOliLogger.debug(funSignature + "processing vertex: " + vertex)
        val allInstancesPerType = getAllInstancesPerTypes(vertex)
        DsOliLogger.debug(funSignature + "allInstancesPerType:")
        allInstancesPerType.foreach {
          instancesPerType =>
            DsOliLogger.debug(funSignature + "type: " + instancesPerType._1.id + ": " + instancesPerType._2)
        }

        // Calculate the combinations
        val allInstancesPerTypeCombinations = combinations(allInstancesPerType)
        DsOliLogger.debug(funSignature + "allInstancesPerTypeCombinations:")
        allInstancesPerTypeCombinations.foreach {
          instancesPerType =>
            DsOliLogger.debug(funSignature + "type: " + instancesPerType._1._1.id + "," + instancesPerType._2._1.id + ": ")
        }

        // Check the compatibility of the DSI type instances
        allInstancesPerTypeCombinations.foreach {
          combinationTuple =>
            val ((firstType, firstInstances), (secondType, secondInstances)) = combinationTuple
            DsOliLogger.debug(funSignature + "testing: firstType: " + firstType + " and secondType: " + secondType)
            DsOliLogger.debug(funSignature + "testing: firstType.primitiveTypes: " + firstType.primitiveTypes
              + " and secondType.primitiveTypes: " + secondType.primitiveTypes)
            DsOliLogger.debug(funSignature + "testing: firstType.linkageOffset: " + firstType.linkageOffset
              + " and secondType.linkageOffset: " + secondType.linkageOffset)
            if (firstInstances.forall {
              firstOffset =>
                secondInstances.forall {
                  secondOffset =>
                    isCompatible(firstType, firstOffset, secondType, secondOffset)
                }
            }) {
              DsOliLogger.debug(funSignature + " compatible: firstType: " + firstType + " secondType: " + secondType)
              vertex.addTypeCompatibility(firstType, secondType, true)
            } else {
              DsOliLogger.debug(funSignature + " not compatible: firstType: " + firstType + " secondType: " + secondType)
              vertex.addTypeCompatibility(firstType, secondType, false)
            }
        }
    }
  }

  /**
   * Do the second global consistency check that compares all DSI type combinations,
   * resulting in a global type matrix representing the compatiblity between types
   *
   * @param mergedTypeGraph the merged type graph
   * @param allDSITypes all DSI types
   * @return a hash map representing the global type matrix
   */
  def compatibleTypesStage2(mergedTypeGraph: DsOliGraph, allDSITypes: ListBuffer[DSIType]): HashMap[(DSIType, DSIType), Boolean] = {
    val funSignature = classSignature + "compatibleTypesStage2: "
    DsOliLogger.debug(funSignature + "entered")
    val globalTypeMatrix = new HashMap[(DSIType, DSIType), Boolean]
    val allDSITypeCombinations = combinations(allDSITypes.toList.sortBy(_.id))

    allDSITypeCombinations.foreach {
      typeCombination =>

        val (firstType, secondType) = typeCombination
        DsOliLogger.debug(funSignature + "testing combination: " + firstType.id + ", " + secondType.id)
        globalTypeMatrix.put((firstType, secondType), true)

        // Test all vertices to see if we find inconsistencies
        if (mergedTypeGraph.graph.nodes.iterator.exists {
          nodeOuter =>
            val vertex = nodeOuter.value.asInstanceOf[DsOliTypeVertex]
            // Need to check both combinations first,second and second,first
            if (vertex.typeMatrix.contains((firstType, secondType))) {
              // the types are not compatible
              vertex.typeMatrix.get((firstType, secondType)).get == false
            } else if (vertex.typeMatrix.contains((secondType, firstType))) {
              // the types are not compatible
              vertex.typeMatrix.get((secondType, firstType)).get == false
            } else {
              // vertex does not contain any type information 
              // -> return false (!) to indicate, that we have not found conflicting type information
              false
            }
        }) {
          DsOliLogger.debug(funSignature + "found incompatible type information")
          globalTypeMatrix.put((firstType, secondType), false)
        }

    }

    return globalTypeMatrix
  }

  /**
   * Fetch the compatible types for a given type
   *
   * @param dsiType the type to compare against
   * @param globalTypeMatrixList the global type compatibility matrix as a list
   * @param globalTypeMatrix the global type matrix as a hash map
   * @return an array of compatible DSI types
   */
  def getCompatibleTypesForTypeList(dsiType: DSIType, globalTypeMatrixList: List[((DSIType, DSIType), Boolean)],
    globalTypeMatrix: HashMap[(DSIType, DSIType), Boolean]): Array[DSIType] = {
    // Always need to check first / second element
    val retList = globalTypeMatrixList.sortBy(matrixTuple => matrixTuple._1._1.id).foldLeft(new ListBuffer[DSIType]) { (list, item) =>
      val ((firstType, secondType), compatible) = item
      // Both types need to be self compatible:
      // - A type gets created by mapping between target and source
      // - Each possible combination is a own type in itself
      // - Each possible sub chunk of such a type is indicated by the same group id
      // - Types with the same group id are never compatible
      // - Types instances which get propagated and are overlapping are always incompatible
      val selfCompatibleFirst = isTypeSelfCompatible(firstType, globalTypeMatrix)
      val selfCompatibleSecond = isTypeSelfCompatible(secondType, globalTypeMatrix)
      if (selfCompatibleFirst && selfCompatibleSecond) {
        // Choose which type to add: we always need to choose the opposite one, i.e. the  one we are not currently looking for
        if (compatible && firstType == dsiType) {
          list.append(secondType)
        } else if (compatible && secondType == dsiType) {
          list.append(firstType)
        } // Default: do nothing
      }
      list
    }
    retList.sortBy(dsiType => dsiType.id).toArray

  }

  /**
   * Fetch the type pair, which is inconsistent
   *
   * @param typeRow array of DSI types
   * @param localTypeMatrix the local compatibility type matrix
   * @retun Option of indices of the incompatible types
   */
  def getFirstInconsistantTypePair2(typeRow: Array[DSIType], localTypeMatrix: Array[Boolean]): Option[(Int, Int)] = {
    val funSignature = classSignature + "getFirstInconsistantTypePair2: "
    var i = 0
    var u = 0
    while (i < typeRow.length) {
      u = i + 1
      while (u < typeRow.length) {
        val firstType = typeRow(i)
        val secondType = typeRow(u)
        // typeRow has a fixed length, empty elements are indicated with null
        if (firstType != null && secondType != null) {
          val index = i * typeRow.length + u
          // We are checking for inconsistent types -> false
          if (localTypeMatrix(index) == false) return Some(i, u)
        }
        u += 1
      }
      i += 1
    }
    None
  }

  /**
   * Check the current hypothesis against the collection of
   * type hypothesis to see if we already have recorded it.
   *
   * @param typeRow the current hypothesis
   * @param curHypotheses the collection of already found hypotheses
   * @return Boolean
   */
  def hypothesisAlreadyPresent3(typeRow: List[DSIType], curHypotheses: ListBuffer[List[DSIType]]): Boolean = {
    curHypotheses.exists {
      hypothesis =>

        // We can already skip hypothesis which are not of the same length
        if (hypothesis.length != typeRow.length) return false

        // All types from the hypothesis are found or compatible
        var identical = true
        for (i <- 0 until hypothesis.length) {
          val hypType = hypothesis(i)
          val rowType = typeRow(i)
          // Check through the various mapping possibilities
          if ((rowType == null && hypType != null) ||
            (rowType != null && hypType == null) ||
            (rowType != null && hypType != null && rowType.id != hypType.id)) {
            identical = false
          }
        }
        identical
    }
  }

  // Debug purpose
  var recurseCounter = 0

  /**
   * Recursively calculate a type hypothesis by obeying the global type matrix
   *
   * @param typeRow the current type row
   * @param globalTypeMatrix the global type matrix as a hash map
   * @param localTypeMatrix the local type matrix (obsolete)
   * @param index the current index iterating through the type row on each recursion
   * @param crossOut obsolete
   * @param curHypotheses storage for the found hypotheses
   * @param padding debug
   */
  def calculateHypothesisRec32(typeRow: Array[DSIType], globalTypeMatrix: HashMap[(DSIType, DSIType), Boolean],
    localTypeMatrix: Array[Boolean], index: Int, crossOut: Boolean,
    curHypotheses: ListBuffer[List[DSIType]], padding: String): Unit = {
    val funSignature = classSignature + "calculateHypothesisRec32: "
    recurseCounter += 1
    DsOliLogger.debug(padding + funSignature + "entered: recurseCounter: " + recurseCounter + " " +
      typeRow.foldLeft("")((string, item) => string + (if (item == null) "null" else item.id) + ","))

    // Add the found data type, if we have hit the end
    if (index >= typeRow.length) {
      DsOliLogger.debug(padding + funSignature + "type row hypothesis: " +
        typeRow.foldLeft("")((string, item) => string + (if (item == null) "null" else item.id) + ","))
      if (!hypothesisAlreadyPresent3(typeRow.toList, curHypotheses)) {
        DsOliLogger.debug(padding + funSignature + "Adding type row: " +
          typeRow.foldLeft("")((string, item) => string + (if (item == null) "null" else item.id) + ","))
        curHypotheses.append(typeRow.toList)
      } else {
        DsOliLogger.debug(padding + funSignature + "type row hypothesis already present")
      }
      return
    }

    var i = index
    val typeRowClone = typeRow.clone
    val firstType = typeRow(i)
    // Only do checks, if current type is not already crossed out
    if (firstType != null) {
      var u = i + 1
      while (u < typeRow.length) {
        val secondType = typeRow(u)
        // typeRow has a fixed length, empty elements are indicated with null
        if (secondType != null) {
          // We are checking for inconsistent types -> false
          if (isCompatibleGlobalMatrix(firstType, secondType, globalTypeMatrix) == false) {
            typeRowClone(u) = null
          }
        }
        u += 1
      }
    }
    // Continue recursively with next type, i.e. move index one position forward
    calculateHypothesisRec32(typeRowClone, globalTypeMatrix, localTypeMatrix, i + 1, crossOut = false, curHypotheses, padding + " ")
  }

  /**
   * Check if two DSI types are globally compatible
   *
   * @param firstType the first DSI type
   * @param secondType the second DSI type
   * @param globalTypeMatrix the global compatibility matrix
   * @return Boolean
   */
  def isCompatibleGlobalMatrix(firstType: DSIType, secondType: DSIType, globalTypeMatrix: HashMap[(DSIType, DSIType), Boolean]): Boolean = {
    // Need to check in both directions, as matrix contains only one direction of the mapping
    val firstSecondOpt = globalTypeMatrix.get((firstType, secondType))
    val first = (firstSecondOpt.isDefined && firstSecondOpt.get)
    val secondFirstOpt = globalTypeMatrix.get((secondType, firstType))
    val second = (secondFirstOpt.isDefined && secondFirstOpt.get)
    first || second
  }

  /**
   * Calculate the type hypothesis
   *
   * @param typeRow the current DSI type row
   * @param globalTypeMatrix the global type matrix as a hash map
   * @param localTypeMatrix the local type matrix (obsolete)
   * @param index the current index iterating through the type row on each recursion
   * @param crossOut remove previous types from the type row
   * @param curHypotheses storage for the found hypotheses
   * @param padding debug
   */
  def calculateHypothesisRec3(typeRow: Array[DSIType], globalTypeMatrix: HashMap[(DSIType, DSIType), Boolean],
    localTypeMatrix: Array[Boolean], index: Int, crossOut: Boolean,
    curHypotheses: ListBuffer[List[DSIType]], padding: String): Unit = {
    val funSignature = classSignature + "calculateHypothesisRec3: "
    recurseCounter += 1
    DsOliLogger.debug(padding + funSignature + "entered: recurseCounter: " + recurseCounter +
      " index = " + index + ": " + typeRow.foldLeft("")((string, item) => string + (if (item == null) "null" else item.id) + ","))

    var i = index
    // High level loop which iterates over all possible type combinations
    while (i < typeRow.length) {

      DsOliLogger.debug(padding + funSignature + " iterate current i: " + i)
      // Cross out previous types
      if (crossOut && i > 0) {
        var x = i - 1
        while (x >= 0) {
          typeRow(x) = null
          x -= 1
        }
      }

      val typeRowClone = typeRow.clone
      val firstType = typeRow(i)

      // Check which types are compatible with current type
      // Only do checks, if current type is not already crossed out
      if (firstType != null) {
        var u = i + 1
        while (u < typeRow.length) {
          val secondType = typeRow(u)
          // typeRow has a fixed length, empty elements are indicated with null
          if (secondType != null) {
            // We are checking for inconsistent types -> false
            if (isCompatibleGlobalMatrix(firstType, secondType, globalTypeMatrix) == false) {
              typeRowClone(u) = null
            }
          }
          u += 1
        }
      }

      // Pass the current type row to the recursive part which checks compatibility of remaining types in the row
      calculateHypothesisRec32(typeRowClone, globalTypeMatrix, localTypeMatrix, i + 1, crossOut = false, curHypotheses, padding + " ")
      DsOliLogger.debug(padding + funSignature + " returned from recursion current i: " + i)

      i += 1
    }
  }

  /**
   * Create the local compatibility type matrix
   *
   * @param typeRow the type row from which to create the local compatibility matrix
   * @param globalTypeMatrix the global type matrix
   * @return the array indicating compatibility between the local types
   */
  def createLocalTypeMatrix(typeRow: Array[DSIType], globalTypeMatrix: HashMap[(DSIType, DSIType), Boolean]): Array[Boolean] = {

    // Create the type matrix as an array
    val localTypeMatrix = new Array[Boolean](typeRow.size * typeRow.size)

    var i = 0
    var u = 0
    // Check through the different combinations
    while (i < typeRow.length) {
      u = i + 1
      while (u < typeRow.length) {
        val firstType = typeRow(i)
        val secondType = typeRow(u)
        val firstSecondOpt = globalTypeMatrix.get((firstType, secondType))
        val first = (firstSecondOpt.isDefined && firstSecondOpt.get)
        val secondFirstOpt = globalTypeMatrix.get((secondType, firstType))
        val second = (secondFirstOpt.isDefined && secondFirstOpt.get)
        val index = i * typeRow.size + u
        localTypeMatrix(index) = first || second
        u += 1
      }
      i += 1
    }

    localTypeMatrix
  }

  /**
   * Checks, if the type is compatible with itself
   *
   * @param dsiType the type to check
   * @param globalTypeMatrix the global type matrix
   * @return Boolean
   */
  def isTypeSelfCompatible(dsiType: DSIType, globalTypeMatrix: HashMap[(DSIType, DSIType), Boolean]): Boolean = {
    val funSignature = classSignature + "isTypeSelfCompatible: "
    // Check, if we need to consider this type at all -> it must be self compatible
    val selfCompatible = globalTypeMatrix.filter {
      typeTuple =>
        val ((firstType, secondType), compatible) = typeTuple
        // Both types need to be the same and they need to marked compatible -> true
        firstType == dsiType && secondType == dsiType && compatible == true
    }
    if (selfCompatible.size > 0) {
      DsOliLogger.debug(funSignature + "id: " + dsiType.id + " selfCompatible: true; selfCompatible.size: " + selfCompatible.size)
    } else {
      DsOliLogger.debug(funSignature + "id: " + dsiType.id + " selfCompatible: false; selfCompatible.size: " + selfCompatible.size)
    }
    // If the filter did not find elements with the specified configuration, return incompatible -> false
    selfCompatible.size > 0
  }

  /**
   * Check through all found DSI types, fetch all globally compatible types for each type
   * and then create a type hypothesis for each.
   *
   * @param allDSITypes all found DSI types
   * @param globalTypeMatrix the global type matrix storing the compatibility information
   * @return a list of all created hypotheses (where a hypothesis is a list of DSI types)
   */
  def calculateAllHypothesis(allDSITypes: ListBuffer[DSIType], globalTypeMatrix: HashMap[(DSIType, DSIType), Boolean]) = {
    val funSignature = classSignature + "calculateAllHypothesis: "
    DsOliLogger.debug(funSignature + "entered: allDSITypes.size: " + allDSITypes.size)
    DsOliLogger.debug(funSignature + "globalTypeMatrix.size: " + globalTypeMatrix.size)

    val globalTypeMatrixList = globalTypeMatrix.toList
    val allHypotheses = new ListBuffer[List[DSIType]]

    // Cycle through all DSI types
    allDSITypes.sortBy(_.id).foreach {
      dsiType =>
        val curHypotheses = new ListBuffer[List[DSIType]]
        // Check, if we need to consider this type at all -> it must be self compatible
        if (isTypeSelfCompatible(dsiType, globalTypeMatrix)) {

          // Fetch all the compatible types for the current type
          val typeRow = getCompatibleTypesForTypeList(dsiType, globalTypeMatrixList, globalTypeMatrix)

          print("type row: ")
          typeRow.foreach {
            dsiType =>
              print(dsiType + ",")
          }
          println()

          // Calculate the local type matrix
          val localTypeMatrix = createLocalTypeMatrix(typeRow, globalTypeMatrix)

          // Debug
          DsOliLogger.debug(funSignature + "typeRow for type : " + dsiType.id)
          DsOliLogger.debug(funSignature + "localTypeMatrix.size: " + localTypeMatrix.size)
          typeRow.foreach {
            dsiRowType =>
              DsOliLogger.debug(funSignature + "dsiRowType: " + dsiRowType.id + " len: " + dsiRowType.primitiveTypes.size)
          }
          DsOliLogger.debug(funSignature + "localTypeMatrix: ")
          var i = 0
          localTypeMatrix.foreach {
            compatible =>
              DsOliLogger.debug(compatible + ",")
              i += 1
              if (i % typeRow.length == 0) {
                DsOliLogger.debug("")
              }

          }
          DsOliLogger.debug(funSignature + "typeRow.size: " + typeRow.size)
          DsOliLogger.debug(funSignature + "typeRow.size: " + typeRow.size)
          DsOliLogger.debug("inital type row: " + typeRow.foldLeft("")((string, item) => string + (if (item == null) "null," else "" + item.id) + ","))

          // Recursively calculate type hypotheses
          recurseCounter = 0
          calculateHypothesisRec3(typeRow, globalTypeMatrix, localTypeMatrix, index = 0, crossOut = true, curHypotheses, "")

          curHypotheses.foreach {
            curHypothesis =>
              if (!hypothesisAlreadyPresent3(curHypothesis, allHypotheses)) {
              }
              allHypotheses.append(curHypothesis)
          }
        }

    }

    // Debug
    /*println(funSignature + "\tsize of allHypotheses: " + allHypotheses.length)
    allHypotheses.sortBy { sublist =>
      sublist.count(_ != null)
    }.reverse.foreach {
      types =>
        println(funSignature + "Hypothesis: ")
        types.foreach {
          typeFragment =>
            println(funSignature + "\ttype fragement: " + typeFragment)
        }
    }*/

    // Remove duplicates
    val mergedHypotheses = mergeHypotheses(allHypotheses.sortBy { sublist =>
      sublist.count(_ != null)
    }.reverse)
    
    
    // Debug
    /*println(funSignature + "\n\n\tsize of mergedHypotheses: " + mergedHypotheses.length)
    mergedHypotheses.sortBy { sublist =>
      sublist.count(_ != null)
    }.reverse.foreach {
      types =>
        println(funSignature + "Hypothesis: ")
        types.foreach {
          typeFragment =>
            println(funSignature + "\ttype fragement: " + typeFragment)
        }
    }
    DsOliLogger.debug(funSignature + "\n\n\tsize of mergedHypotheses: " + mergedHypotheses.length)
    DsOliLogger.debug(funSignature + "\tsize of allHypotheses: " + allHypotheses.length)
    */
    

    // Return the [merged] hypotheses
    mergedHypotheses
  }

  /**
   * Check if the given hypothesis is covered by an already
   * stored hypothesis.
   *
   * @param hypothesis the hypothesis to check
   * @param presentHypotheses all saved hypotheses
   * @return Boolean
   */
  def isSubsumed(hypothesis: List[DSIType], presentHypotheses: ListBuffer[List[DSIType]]): Boolean = {
    // Cycle through all stored hypotheses
    presentHypotheses.exists {
      presentHypothesis =>
        presentHypothesis != hypothesis &&
          // For all DSI types of the hypothesis there
          // must exist an equivalent type inside of 
          // the current already stored hypothesis
          hypothesis.forall {
            hypType =>
              // Check through all DSI types of the current
              // already stored hypothesis
              presentHypothesis.exists {
                presentHypType =>
                  hypType == presentHypType
              }
          }
    }
  }

  /**
   * Try to merge as many hypotheses as possible by checking, if one
   * hypothesis is subsumed by another (i.e., there exists an
   * hypothesis which partially or fully covers another hypothesis)
   *
   * @param allHypotheses list of the created hypotheses
   * @return a list of all hypotheses where duplicates are removed
   *
   */
  def mergeHypotheses(allHypotheses: ListBuffer[List[DSIType]]): ListBuffer[List[DSIType]] = {
    val mergedHypotheses = new ListBuffer[List[DSIType]]
    allHypotheses.foreach {
      hypothesis =>
        // If there does not exist a type in the merged hypotheses
        // which contains all the types of the current hypothesis
        // add the type
        if (!isSubsumed(hypothesis, mergedHypotheses)) {
          mergedHypotheses.append(hypothesis)

        }
    }
    mergedHypotheses
  }

  // The typed structs start at this offset.
  // All automatically created types within the binary fronted
  // can live in the space between zero and idOffset.
  val idOffset = 1000

  /**
   * Write out all the nested types recursively. Nested types are stored
   * in a list of descending order.
   *
   * @param nestedTypesDsc the nested types stored in descending order according to their size
   * @param index the current index of the nesting
   * @param offsetRec the offset within the type
   * @param padding debug
   * @param writer the stream to write to
   * @param firstRec indicate the first recursion depth to calculate the correct offset of the nested struct
   *
   */
  def writeNestingRec(nestedTypesDsc: ListBuffer[(DSIType, Long)], index: Int, offsetRec: Long, padding: String, writer: Writer, firstRec: Boolean): Unit = {
    val funSignature = classSignature + "writeNestingRec: "

    DsOliLogger.debug(funSignature + "entered: index = " + index)
    val (nestedType, offset) = nestedTypesDsc(index)

    // Offsets are relative to enclosing offset, except for the first time!
    val writeOffset = if (firstRec == true) offset else offset - offsetRec

    // only print this, if it is part of the nesting
    if (nestedTypesDsc(index)._2 >= offsetRec) {

      DsOliLogger.debug(funSignature + padding + "0x" + writeOffset.toHexString + ": struct " + (nestedType.id + idOffset) + "{")
      writer.write(padding + "0x" + writeOffset.toHexString + ": struct " + (nestedType.id + idOffset) + "{\n")
    }
    // Always recurse
    if (nestedTypesDsc.size - 1 > index) {
      writeNestingRec(nestedTypesDsc, index + 1, offset, padding + " ", writer, false)
    }

    // Only print this if it is part of the nesting
    if (nestedTypesDsc(index)._2 >= offsetRec) {
      var skipOffset = (if (nestedTypesDsc.size - 1 > index) nestedTypesDsc(index + 1)._2 - offset else 0)
      var skipSize = (if (nestedTypesDsc.size - 1 > index) nestedTypesDsc(index + 1)._1.byteSize else 0)

      nestedType.primitiveTypes.foreach {
        field =>
          // Skip parts which are covered by a potentially nested struct
          if (field.vOffset < skipOffset || field.vOffset >= skipOffset + skipSize) {
            DsOliLogger.debug(funSignature + padding + "writing because of skipOffset: " + skipOffset +
              ", skipSize: " + skipSize + ", field.vOffset: " + field.vOffset)
            DsOliLogger.debug(funSignature + padding + "        0x" + field.vOffset.toHexString + ": " +
              field.cType.replaceAll(" ", "") + ";")

            writer.write(padding + "        0x" + field.vOffset.toHexString + ": " + field.cType.replaceAll(" ", "") + ";\n")

          } else {
            DsOliLogger.debug(funSignature + padding + "skipping because of skipOffset: " + skipOffset +
              ", skipSize: " + skipSize + ", field.vOffset: " + field.vOffset)
            DsOliLogger.debug(funSignature + padding + "skipping because of skipOffset: " + skipOffset +
              ": " + field.vOffset.toHexString + ": " + field.cType.replaceAll(" ", "") + ";")
          }
      }

      DsOliLogger.debug(funSignature + padding + "}")
      writer.write(padding + "}\n")

      DsOliLogger.debug(funSignature + padding + "returning")
    }
  }

  /**
   * Write out the refined type vertex including found nested type instances
   *
   * @param typeVertex the type vertex to write
   * @param nestedTypeInstancesAsc the nested type instances
   * @param padding debug
   * @param writer the stream to write to
   * @param isStack flag indicating stack or heap
   */
  def writeTypeVertexRefined2(typeVertex: DsOliTypeVertex, nestedTypeInstancesAsc: ListBuffer[(DSIType, Long)], padding: String,
    writer: Writer, isStack: Boolean = false): Unit = {
    val funSignature = classSignature + "writeTypeVertexRefined2: "
    var closeStruct = false
    DsOliLogger.debug(funSignature + "Function = " + typeVertex.typeID)
    // The stack only needs information about the struct, the function caption is put into place
    // at the diffing stage between original and refined stack information
    if (!isStack) {
      writer.write("Function = " + typeVertex.typeID + "\n")
    }

    // If we have nesting, we need to record how many
    // of the following types we need to skip as they
    // are handled by the nested struct.
    var skipOffset = 0L
    var curOffset = 0L

    // First, fetch the top level nested structs inside of this vertex (the ones, which are not nested themselves)
    val nestedTypeInstancesAtVertexLevel = nestedTypeInstancesAsc.filter { case (dsiType, offset) => dsiType.isNested == false }

    // Get the biggest element to check, if we got a complete vertex coverage
    val nestedTypesWithinOffsetDsc = nestedTypeInstancesAtVertexLevel.sortBy(_._1.byteSize).reverse

    val structID = if (nestedTypesWithinOffsetDsc.head._1.byteSize == typeVertex.getSize) " " + (typeVertex.id + idOffset) else ""
    val minusOffsetForStack = if (isStack) "-0x" + typeVertex.offsetFromStart.toHexString + ": " else ""
    DsOliLogger.debug(funSignature + minusOffsetForStack + "struct" + structID + "{")
    writer.write(minusOffsetForStack + "struct" + structID + "{\n")
    // End of complete type vertex coverage

    // Flatten the current type vertex, and totally rely on the refined types 
    // Howard types are ignored, as they are not taken into consideration during the refinement anyways
    val typeSeq = getPrimitiveTypesFlattenedRec(typeVertex.vType, 0)

    //typeVertex.vType.fields.foreach {
    typeSeq.foreach {
      field =>
        DsOliLogger.debug(funSignature + field + " : field.vOffset: " + field.vOffset + " skipOffset: " + skipOffset + " curOffset: " + curOffset)

        // First we need to check, if this field is covered by a nested struct already
        if (field.vOffset >= skipOffset) {

          // Does there exist a nested type which covers this particular offset?
          val nestedTypesWithinOffset = nestedTypeInstancesAsc.filter {
            case (dsiType, offset) =>
              DsOliLogger.debug(funSignature + " calculate nested elements: " +
                "field.vOffset : " + field.vOffset +
                " offset: " + offset +
                " dsiType.byteSize: " + dsiType.byteSize +
                " field.vOffset >= offset: " + (field.vOffset >= offset) +
                " field.vOffset < offset + dsiType.byteSize: " + (field.vOffset < offset + dsiType.byteSize))

              field.vOffset >= offset && field.vOffset < offset + dsiType.byteSize
          }

          val nestedTypesWithinOffsetDsc = nestedTypesWithinOffset.sortBy(_._1.byteSize).reverse

          if (nestedTypesWithinOffset.size > 0) {
            DsOliLogger.debug(funSignature + " found a nested type which covers the current field: ")
            val (nestedDSIType, nestedOffset) = nestedTypesWithinOffsetDsc.head
            skipOffset = nestedOffset + nestedDSIType.byteSize
            DsOliLogger.debug(funSignature + "calling writeNestinRec: nestedTypesWithinOffsetDsc: " + nestedTypesWithinOffsetDsc + "\n")
            writeNestingRec(nestedTypesWithinOffsetDsc, 0, nestedOffset, padding, writer, true)

          } else {
            // Else write the field
            DsOliLogger.debug(funSignature + " found no nested type which covers the current field: write the field ")
            skipOffset += field.vSize

            // Compound or primitive field?
            if (field.fType == Compound) {
              val recType = typeDB.getTypeObject(field.cType).get
              printTypeWithOffset(recType, field.vOffset, writer, padding)
            } else {
              writer.write(padding + "0x" + field.vOffset.toHexString + ": " + field.cType.replaceAll(" ", "") + ";\n")
            }
          }
        }
    }
    DsOliLogger.debug(funSignature + "}")
    writer.write("}\n")
    writer.write("\n")
  }

  /**
   * Helper to print a struct
   *
   * @param dsOliType the type to print
   * @param writer the stream to write to
   * @param padding indentation
   */
  def printType(dsOliType: DsOliType, writer: PrintWriter, padding: String): Unit = {
    writer.println(padding + "struct{")
    printTypeVertexRec(dsOliType: DsOliType, writer: PrintWriter, padding + " ")
    writer.println(padding + "}")
  }

  /**
   * Helper to print a struct with offset, used for nested structs
   *
   * @param dsOliType the type to print
   * @param offset the offset of the struct
   * @param writer the stream to write to
   * @param padding indentation
   */
  def printTypeWithOffset(dsOliType: DsOliType, offset: Long, writer: Writer, padding: String, isStack: Boolean = false): Unit = {
    val minusOffsetForStack = if (isStack) "-" else ""
    writer.write(padding + minusOffsetForStack + "0x" + offset.toHexString + ": struct{\n")
    printTypeVertexRec(dsOliType: DsOliType, writer: Writer, padding + " ")
    writer.write(padding + "}\n")
  }

  /**
   * Recursively print a type vertex
   *
   * @param dsOliType the type to print
   * @param writer the stream to write to
   * @param padding indentation
   */
  def printTypeVertexRec(dsOliType: DsOliType, writer: Writer, padding: String): Unit = {
    dsOliType.fields.foreach {
      field =>
        if (field.fType == Compound) {
          writer.write(padding + "0x" + field.vOffset.toHexString + ": struct{\n")
          val recType = typeDB.getTypeObject(field.cType).get
          printTypeVertexRec(recType, writer, padding + "  ")
          writer.write(padding + "}\n")
        } else {
          writer.write(padding + "0x" + field.vOffset.toHexString + ": " + field.cType.replaceAll(" ", "") + ";\n")
        }
    }
  }

  /**
   * Write the merge info used by the DSI PIN tool
   *
   * @param mergedTypesList list of merged types
   * @param fileName name of the merge file
   */
  def writeMergedTypesList(mergedTypesList: ListBuffer[String], fileName: String): Unit = {
    val suffix = ".mergeinfo"
    val fileAppend = true
    // Only write, if there actually was a merge, i.e., more than one element in the list
    if (mergedTypesList.size > 1) {
      val writer = new PrintWriter(new FileWriter(DsOliPathUtils.getPath + DsOliPathUtils.getRefinedTypesDir + fileName + suffix, fileAppend))
      writer.println(mergedTypesList.foldLeft("")((item, string) => string + "," + item))
      writer.close()
    }
  }

  /**
   * Debug helper: prints the stack info
   *
   * @param stackFileMapping HashMap "function-id" -> HashMap "offset" -> type-string
   */
  def printStackMapping(stackFileMapping: HashMap[String, HashMap[Long, String]]) = {
    stackFileMapping.keys.foreach {
      key =>
        DsOliLogger.debug(key + ":")
        val keyElements = stackFileMapping.get(key).get
        keyElements.keys.foreach {
          keyElemKey =>
            DsOliLogger.debug("\t" + keyElemKey + ": " + keyElements.get(keyElemKey).get)
        }
    }

  }

  /**
   * Debug helper: prints the heap info
   *
   * @param stackFileMapping HashMap "function-id" -> type-string
   */
  def printHeapMapping(heapFileMapping: HashMap[String, String]) = {
    heapFileMapping.keys.foreach {
      key =>
        DsOliLogger.debug(key + ":" + heapFileMapping.get(key).get)
    }
  }

  /**
   * Write out the refined stack by calculating the diff between the original
   * and the new stack
   *
   * @param origMapping the original stack layout
   * @param refineMapping the new refined stack layout
   * @param stackWriter stream to the stack file to write
   */
  def diffStack(origMapping: HashMap[String, HashMap[Long, String]], refinedMapping: HashMap[String, HashMap[Long, String]], stackWriter: Writer) = {
    // Iterate all original elements
    origMapping.keys.foreach {
      funId =>
        DsOliLogger.debug(funId + ":")
        stackWriter.write("Function = " + funId + "\n")
        val origStackElements = origMapping.get(funId).get

        // New line, if stack has no elements
        if (origStackElements.size == 0) {
          stackWriter.write("\n")
        }

        // Diff between orig and refined
        origStackElements.keys.toList.sorted.foreach {
          offset =>
            DsOliLogger.debug("\t" + offset + ": " + origStackElements.get(offset).get)
            // Does the current stack exist in the refinement and does there exist a refinement at this offset?
            // Yes -> use refinement
            // No -> use orig
            val printableStackElem = if (refinedMapping.contains(funId) && refinedMapping.get(funId).get.contains(offset)) {
              val refinedStack = refinedMapping.get(funId).get
              refinedStack.get(offset).get
            } else {
              origStackElements.get(offset).get
            }
            stackWriter.write(printableStackElem + "\n")
        }

        // Always new line in the end
        stackWriter.write("\n")
    }
  }

  /**
   * Write out the heap elements which were not refined by
   * calculating the diff between the original and the new heap
   *
   * @param origMapping the original heap layout
   * @param refineMapping the new refined heap layout
   * @param writer stream to the heap file to write
   */
  def diffHeap(origMapping: HashMap[String, String], refinedMapping: ListBuffer[String], writer: Writer) = {
    // Iterate all original elements
    origMapping.keysIterator.foreach {
      funId =>
        DsOliLogger.debug(funId + ":")

        // If element was not written refined: funId of orig is not found in refined mapping
        if (refinedMapping.find(refId => refId == funId).isEmpty) {
          writer.write("Function = " + funId + "\n")
          val origStructString = origMapping.get(funId).get
          writer.write(origStructString)
        }

        // Always new line in the end
        writer.write("\n")
    }
  }

  /**
   * Parse the original heap file and keep a mapping.
   * This mapping will be used to write out the
   * unmodified parts of the heap later on
   *
   * @return a HahsMap with the function id as key and the type string as value
   */
  def parseOriginalHeapFile() = {
    val funSignature = classSignature + "parseOriginalHeapFile: "
    // HashMap "fun-id" -> type-string 
    val heapFileMapping = new HashMap[String, String]
    val fileName = DsOliPathUtils.getPath + DsOliPathUtils.getProjectName + ".taint"

    // Small state machine
    // State: search for function
    val searchFun = 0
    // State: found function, i.e., inside function
    val insideFun = 1
    // State: found a struct
    val foundStruct = 2
    var curState = searchFun
    var numberOfBraces = 0
    var curId = ""
    for (line <- Source.fromFile(fileName).getLines()) {
      DsOliLogger.debug(line)
      // Start out by searching for a function
      if (curState == searchFun) {
        // Switch state -> inside function
        if (line.startsWith("Function")) {
          val splitted = line.split(" = ")
          curId = splitted(1)
          DsOliLogger.debug(funSignature + " found function: searchFun -> insideFun: id =" + curId + ": " + line)
          heapFileMapping.put(curId, "")
          curState = insideFun
        }
        // State: inside function
      } else if (curState == insideFun) {
        // Empty line indicates end of function: switch state -> search function
        if (line.size <= 2) {
          DsOliLogger.debug(funSignature + " found empty line: insideFun -> searchFun")
          curState = searchFun
          // Found struct: switch state -> found struct
        } else if (line.contains("struct")) {
          DsOliLogger.debug(funSignature + " found struct line: insideFun -> foundStruct:  " + line)
          curState = foundStruct
          heapFileMapping.put(curId, line)
          numberOfBraces = 1
        } else {
          throw new Exception(funSignature + " found primitive line: insideFun -> insideFun:  " + line)
        }
        // State: found struct
      } else if (curState == foundStruct) {

        // Save the content of the struct by gluing each line together
        heapFileMapping.put(curId, heapFileMapping.get(curId).get + "\n" + line)

        // Count opening/closing braces: switch state if brace count is zero -> inside function
        if (line.contains("{")) {
          numberOfBraces += 1
          DsOliLogger.debug(funSignature + " found { (" + numberOfBraces + ") line: " + line)
        } else if (line.contains("}")) {
          numberOfBraces -= 1
          DsOliLogger.debug(funSignature + " found } (" + numberOfBraces + ") line: " + line)
        }
        if (numberOfBraces == 0) {
          curState = insideFun
          DsOliLogger.debug(funSignature + " end of struct (" + numberOfBraces + ") line: foundStruct -> insideFun " + line)
        }
      }
    }
    DsOliLogger.debug("Parse mapping: ")
    printHeapMapping(heapFileMapping)
    heapFileMapping
  }

  /**
   * Parse the original stack file and keep a mapping.
   * This mapping will be used to write out the
   * unmodified parts of the stack later on
   *
   * @return a HashMap "fun-id" -> HashMap "offset" -> type-string
   */
  def parseOriginalStackFile() = {
    val funSignature = classSignature + "parseOriginalStackFile: "
    // HashMap "fun-id" -> HashMap "offset" -> type-string 
    val stackFileMapping = new HashMap[String, HashMap[Long, String]]
    val fileName = DsOliPathUtils.getPath + DsOliPathUtils.getProjectName + ".stack"

    // State machine
    // State: search for function
    val searchFun = 0
    // State: inside function
    val insideFun = 1
    // State: found struct
    val foundStruct = 2
    var curState = searchFun
    var numberOfBraces = 0
    var curId = ""
    var curHashMap: HashMap[Long, String] = null
    var lastOffset = 0
    for (line <- Source.fromFile(fileName).getLines()) {
      DsOliLogger.debug(line)
      // Start in state search function
      if (curState == searchFun) {
        // Switch state -> inside function
        if (line.startsWith("Function")) {
          val splitted = line.split(" = ")
          curId = splitted(1)
          DsOliLogger.debug(funSignature + " found function: searchFun -> insideFun: id =" + curId + ": " + line)
          curHashMap = new HashMap[Long, String]()
          stackFileMapping.put(curId, curHashMap)
          curState = insideFun
        }
        // State: inside of function
      } else if (curState == insideFun) {
        val prefix = 3
        val curOffset = line.split(": ")(0)
        val curOffsetVal = if (curOffset.size > 0) {
          Integer.parseInt(curOffset.substring(prefix), 16)
        } else {
          0
        }
        lastOffset = curOffsetVal

        // Empty line indicates end of function: switch state -> search function
        if (line.size <= 2) {
          DsOliLogger.debug(funSignature + " found empty line: insideFun -> searchFun")
          curState = searchFun
          // Found struct: switch state -> found struct
        } else if (line.contains("struct")) {
          DsOliLogger.debug(funSignature + " found struct line: ." + curOffsetVal + ". insideFun -> foundStruct:  " + line)
          curState = foundStruct
          curHashMap.put(curOffsetVal, line)
          numberOfBraces = 1
        } else {
          curHashMap.put(curOffsetVal, line)
          DsOliLogger.debug(funSignature + " found primitive line: ." + curOffsetVal + ". insideFun -> insideFun:  " + line)
        }
        // State: found struct
      } else if (curState == foundStruct) {

        // Save the content of the struct by gluing each line together
        curHashMap.put(lastOffset, curHashMap.get(lastOffset).get + "\n" + line)

        // Count opening/closing braces: switch state if brace count is zero -> inside function
        if (line.contains("{")) {
          numberOfBraces += 1
          DsOliLogger.debug(funSignature + " found { (" + numberOfBraces + ") line: " + line)
        } else if (line.contains("}")) {
          numberOfBraces -= 1
          DsOliLogger.debug(funSignature + " found } (" + numberOfBraces + ") line: " + line)
        }
        if (numberOfBraces == 0) {
          curState = insideFun
          DsOliLogger.debug(funSignature + " end of struct (" + numberOfBraces + ") line: foundStruct -> insideFun " + line)
        }
      }
    }
    DsOliLogger.debug("Parse mapping: ")
    printStackMapping(stackFileMapping)
    stackFileMapping
  }

  /**
   * Write exactly one type hypothesis out. This involves reading
   * in the stack and heap files, writing the refined parts and
   * the unmodified parts as well as the merge info
   *
   * @param typeHypothesis the type hypothesis to write
   * @param mergedTypeGraph the merged type graph
   * @param id a unique id
   */
  def writeOneHypothesisToFile(typeHypothesis: List[DSIType], mergedTypeGraph: DsOliGraph, id: Int): Unit = {
    val funSignature = classSignature + "writeOneHypothesisToFile: "

    // Create some offset
    var i = id

    val parsedHeapFile = parseOriginalHeapFile()
    val parsedStackFile = parseOriginalStackFile()

    DsOliLogger.debug(funSignature + "Evaluating type hypothesis: " + typeHypothesis)

    // Have one global lookup, which stores location site merges
    // DsOliTypeVertex id -> list of instances
    val typeInstancesPerVertex = new HashMap[DsOliTypeVertex, ListBuffer[(DSIType, Long)]]

    val fileName = "types_" + i
    val writer = new PrintWriter(new File(DsOliPathUtils.getPath + DsOliPathUtils.getRefinedTypesDir + fileName + ".taint"))
    val stackWriter = new PrintWriter(new File(DsOliPathUtils.getPath + DsOliPathUtils.getRefinedTypesDir + fileName + ".stack"))

    // Keep refinements of the stack structs
    val stackFileMapping = new HashMap[String, HashMap[Long, String]]
    val heapFileMapping = new ListBuffer[String]

    // Create new type based on hypothesis
    // Write all heap types
    mergedTypeGraph.graph.nodes.iterator.foreach {
      node =>
        val typeVertex = node.value.asInstanceOf[DsOliTypeVertex]
        val nestedTypeInstancesAsc = nestTypes(typeVertex, typeHypothesis)
        // Iterate all elements from the merged type graph which are on the heap
        if (!typeVertex.isStack) {
          DsOliLogger.debug(funSignature + "checking type vertex: " + typeVertex)

          // Keep track of the written heap objects
          heapFileMapping.append(typeVertex.typeID)

          // Maybe the hypothesis has removed all types from this vertex
          if (nestedTypeInstancesAsc.size == 0) {

            // If we have not found any refinements, just dump the type immediately
            DsOliLogger.debug(funSignature + " testing function: " + typeVertex.typeID + " isStack: " + typeVertex.isStack)
            if (typeVertex.typeID != "" && typeVertex.typeID != "-") {
              DsOliLogger.debug(funSignature + " heap: " + typeVertex.typeID + " isStack: " + typeVertex.isStack)
              writer.println("Function = " + typeVertex.typeID)

              val dsOliType = typeDB.getTypeObject(typeVertex.vType.vType).get
              //printTypeVertexRec(dsOliType, writer, "")
              printType(dsOliType, writer, "")

            } else {
              DsOliLogger.debug("skip heap Function = " + typeVertex.typeID + ": " + typeVertex.vType)
            }
          } else {
            // Dump the refined type
            writeTypeVertexRefined2(typeVertex, nestedTypeInstancesAsc, "", writer)

          }
          // Stack
        } else {
          val stackBuffer = new StringWriter()
          // write to stack file
          DsOliLogger.debug(funSignature + " stack: " + typeVertex.typeID + " isStack: " + typeVertex.isStack)
          if (nestedTypeInstancesAsc.size != 0) {
            writeTypeVertexRefined2(typeVertex, nestedTypeInstancesAsc, "", stackBuffer, isStack = true)

            // The ID of stack might already be present, as there might exist multiple struct objects on the stack
            if (!stackFileMapping.contains(typeVertex.typeID)) {
              stackFileMapping.put(typeVertex.typeID, new HashMap[Long, String])
            }
            // Store the refinement of the struct at this particular offset
            stackFileMapping.get(typeVertex.typeID).get.put(typeVertex.offsetFromStart, stackBuffer.toString())

          }
        }

    }

    // Debug
    DsOliLogger.debug(funSignature + "*********************")
    printStackMapping(stackFileMapping)
    DsOliLogger.debug(funSignature + "*********************")
    printStackMapping(parsedStackFile)

    // Write the stack diff
    diffStack(parsedStackFile, stackFileMapping, stackWriter)

    // Debug
    DsOliLogger.debug(funSignature + "*********************")
    heapFileMapping.foreach(id => DsOliLogger.debug("written heap object: " + id))
    writer.write("*************************appended elements**********************\n")

    // Write the heap diff
    diffHeap(parsedHeapFile, heapFileMapping, writer)

    writer.close
    stackWriter.close

  }

  /**
   * Check if two types have the same nested types
   *
   * @param nestedType first type with nested instances
   * @param nestedTypeOff the offset of the first nested instance
   * @param nestedTypeCheck second type with nested instances
   * @param nestedTypeOffCheck the offset of the second nested instance
   */
  def nestedTypesPresent(nestedType: DSIType, nestedTypeOff: Long, nestedTypeCheck: DSIType, nestedTypeCheckOff: Long): Boolean = {
    val funSignature = classSignature + "nestedTypesPresent: "
    println(funSignature + "entered: nestedType: " + nestedType + " nestedTypeCheck: " + nestedTypeCheck)

    // Base case: check the current level
    val topLevelEqual = nestedType.nestedTypeInstances.forall {
      typeInst1Tuple =>
        val (typeInst1, typeInst1Off) = typeInst1Tuple
        // Checks for presence of first type instance in second type
        nestedTypeCheck.nestedTypeInstances.exists {
          typeInst2Tuple =>
            val (typeInst2, typeInst2Off) = typeInst2Tuple
            // Need to check the offsets as well, as instances might not
            // occur at the same offset necessarily (avoids false positives)
            typeInst2.id == typeInst1.id &&
              typeInst2Off - nestedTypeCheckOff == typeInst1Off - nestedTypeOff
        }
    }

    // If top level does not match, no need to check the rest
    if (topLevelEqual == false) {
      println(funSignature + "top level mismatch, return false")
      return false
    }

    // Cycle through each top level element
    nestedType.nestedTypeInstances.foreach {
      typeInst1Tuple =>
        val (typeInst1, typeInst1Off) = typeInst1Tuple

        // Get the corresponding instance, which must exist
        val (corTypeInst, corTypeInstOff) = nestedTypeCheck.nestedTypeInstances.find {
          typeInst2Tuple =>
            val (typeInst2, typeInst2Off) = typeInst2Tuple
            typeInst2.id == typeInst1.id &&
              typeInst2Off - nestedTypeCheckOff == typeInst1Off - nestedTypeOff
        }.get

        println(funSignature + "recurse")
        // Recurse and compare these two types
        val recResult = nestedTypesPresent(typeInst1, typeInst1Off, corTypeInst, corTypeInstOff)
        if (recResult == false) return false
    }
    return true
  }

  /**
   * Checks, if the interpretation already exists
   *
   * @param allInterpretationsForType all interpretations for a particular type
   * @param nestedTypeId the id of the nested type
   * @param nesetedType the nested type
   * @param nestedTypeOffset the offset of the nested type
   */
  def interpretationDoesNotExist(allInterpretationsForType: HashMap[Long, ListBuffer[(DSIType, Long)]], nestedTypeId: Long,
    nestedType: DSIType, nestedTypeOffset: Long): Boolean = {
    val funSignature = classSignature + "interpretationDoesNotExist: "
    println(funSignature + "entered")
    // Id present at all
    if (!allInterpretationsForType.contains(nestedTypeId)) return true
    val listOfInterpretations = allInterpretationsForType.get(nestedTypeId).get
    // Check, that there exists no element in the interpretations, which is exactly the
    // same regarding the elements
    !listOfInterpretations.exists {
      interpretation =>
        // Check if the sizes match and that each element of the nested instances is present
        interpretation._1.nestedTypeInstances.size == nestedType.nestedTypeInstances.size &&
          // Take the offset of the nested instances into consideration to avoid false negatives: nested elements should always be treated relative from their 
          // enclosing type
          nestedTypesPresent(interpretation._1, interpretation._2, nestedType, nestedTypeOffset)
    }
  }

  /**
   * Recursively transforms a nested type instances to a list
   *
   * @param nestedType the type for which the nested elements are converted to a list
   * @param nestedElemsList stores the resutl
   * @param padding debug
   */
  def getNestedInstancesAsListRec(nestedType: DSIType, nestedElemsList: ListBuffer[(DSIType, Long)], padding: String): Unit = {
    val funSignature = padding + classSignature + "getNestedInstancesAsListRec: "
    DsOliLogger.debug(funSignature + "entered: nestedType.id: " + nestedType.id)
    nestedType.nestedTypeInstances.foreach {
      nestedTypeTuple =>
        val (nestTypeInst, nestTypeOff) = nestedTypeTuple
        DsOliLogger.debug(funSignature + "appending nested instance (@" + nestTypeOff + "): " + nestTypeInst.id)
        nestedElemsList.append(nestedTypeTuple)
        getNestedInstancesAsListRec(nestTypeInst, nestedElemsList, padding + "    ")
    }
  }

  /**
   * Transform all nested instances for a type to a list
   *
   * @param nestedType the type for which the nested elements are converted to a list
   * @return the nested types as a list
   */
  def getNestedInstancesAsList(nestedType: DSIType): ListBuffer[(DSIType, Long)] = {
    val nestedElemsList = new ListBuffer[(DSIType, Long)]
    getNestedInstancesAsListRec(nestedType, nestedElemsList, "")
    nestedElemsList
  }

  /**
   * Collect all the nested types
   *
   * @param mergedTypeGraphCopy a copy (!) of the merged type graph
   * @param typeHypothesis the current type hypothesis
   * @param allNestedTypes stores the found nested types
   */
  def collectAllNestedTypes(mergedTypeGraphCopy: DsOliGraph, typeHypothesis: List[DSIType],
    allNestedTypes: ListBuffer[((DSIType, Long), DsOliTypeVertex)]): Unit = {
    val funSignature = classSignature + "collectAllNestedTypes: "
    DsOliLogger.debug(funSignature + "entered")
    // Collect all nested types
    mergedTypeGraphCopy.graph.nodes.iterator.foreach {
      node =>
        val typeVertex = node.value.asInstanceOf[DsOliTypeVertex]
        val nestedTypeInstancesAsc = nestTypes(typeVertex, typeHypothesis)
        DsOliLogger.debug(funSignature + "typeVertex(id: " + typeVertex.id + "): " + typeVertex.vType.vType)
        nestedTypeInstancesAsc.foreach {
          instance =>
            DsOliLogger.debug(funSignature + "type (@off:" + instance._2 + ",typeVertex.id: " + typeVertex.id + "):" + instance._1)
            val (instType, instOff) = instance
            if (instType.nestedTypeInstances.size > 0) {
              DsOliLogger.debug(funSignature + "found type instance with nesting: " + instType.id)
              allNestedTypes.append((instance, typeVertex))
            }
        }
    }

    // Select all types which showed nesting. This takes care of the situation where
    // there exists a type which shows nesting and no nesting at all
    DsOliLogger.debug(funSignature + "searching for missed types showing nesting and no-nesting:")
    mergedTypeGraphCopy.graph.nodes.iterator.foreach {
      node =>
        val typeVertex = node.value.asInstanceOf[DsOliTypeVertex]
        DsOliLogger.debug(funSignature + "typeVertex(id: " + typeVertex.id + "): " + typeVertex.vType.vType)

        // We need to do the nesting first as nestTypes always operates on copies, so nesting
        // calculation is not preserved from previous nestTypes() call
        val nestedTypeInstancesAsc = nestTypes(typeVertex, typeHypothesis)

        // Cycle through each dsi type instance of the particular vertex
        nestedTypeInstancesAsc.foreach {
          typeInstance =>
            val (typeInst, typeOffset) = typeInstance

            DsOliLogger.debug(funSignature + "type (@off:" + typeOffset + ",typeVertex.id: " + typeVertex.id + "): nesting size: " +
              typeInst.nestedTypeInstances.size + ":: " + typeInst)

            // Type instance does not have nesting, but the type is listed as a type showing nesting elsewhere
            if (typeInst.nestedTypeInstances.size == 0 && allNestedTypes.exists {
              nestedTypeInstance =>
                val ((nestedTypeInstCheck, nestedTypeOffsetCheck), nestedTypeVertexCheck) = nestedTypeInstance
                nestedTypeInstCheck.id == typeInst.id
            }) {
              DsOliLogger.debug(funSignature + "adding missed nesting type: " + typeInstance)
              allNestedTypes.append((typeInstance, typeVertex))
            }
        }
    }
  }

  /**
   * Ensure, that all nested types are consistent within each other, i.e.,
   * they all have the same nested types.
   *
   * @param allNestedTypes all the found nested types
   * @param allInterpretationsForType store the resulting consistent nested types
   */
  def ensureConsistencyAmongNestedTypes(allNestedTypes: ListBuffer[((DSIType, Long), DsOliTypeVertex)],
    allInterpretationsForType: HashMap[Long, ListBuffer[(DSIType, Long)]]): Unit = {
    val funSignature = classSignature + "ensureConsistencyAmongNestedTypes: "
    val checkedTypes = new ListBuffer[(((DSIType, Long), DsOliTypeVertex), ((DSIType, Long), DsOliTypeVertex))]
    // Ensure consistency among all nested types 
    allNestedTypes.foreach {
      nestedTypeTuple =>
        val ((nestedType, nestedTypeOff), typeVertex) = nestedTypeTuple
        // Does there exist a type instance which differs in 
        // the elements, which are nested inside of it
        allNestedTypes.foreach {
          nestedTypeCheckTuple =>

            val ((nestedTypeCheck, nestedTypeCheckOff), typeVertexCheck) = nestedTypeCheckTuple

            // Debug
            DsOliLogger.debug(funSignature + "checking nestedType: " + nestedType.id + " from vertex: " + typeVertex.id +
              " against nestedTypeCheck: " + nestedTypeCheck.id + " from vertex: " + typeVertexCheck.id)
            DsOliLogger.debug(funSignature + "\tnestedType elems: " + nestedType.nestedTypeInstances)
            DsOliLogger.debug(funSignature + "\tnestedTypeCheck elems: " + nestedTypeCheck.nestedTypeInstances)

            // Check, if the current combination was already processed
            if (true || checkedTypes.find {
              checkedTypeTuple =>
                val (((checkedType, checkedTypeOff), checkedTypeVertex), ((checkedType2, checkedType2Off), checkedType2Vertex)) = checkedTypeTuple

                // Debug
                DsOliLogger.debug(funSignature + "checkedType.id: " + checkedType.id + " checkedTypeOff: " + checkedTypeOff +
                  " checkedTypeVertex.id: " + checkedTypeVertex.id)
                DsOliLogger.debug(funSignature + "checkedType2.id: " + checkedType2.id + " checkedType2Off: " + checkedType2Off +
                  " checkedType2Vertex.id: " + checkedType2Vertex.id)
                DsOliLogger.debug(funSignature + "nestedType.id: " + nestedType.id + " nestedTypeOff: " + nestedTypeOff +
                  " typeVertex.id: " + typeVertex.id)
                DsOliLogger.debug(funSignature + "nestedTypeCheck.id: " + nestedTypeCheck.id + " nestedTypeCheckOff: " + nestedTypeCheckOff +
                  " typeVertexCheck.id: " + typeVertexCheck.id)

                // Check first -> second
                (checkedType.id == nestedType.id && checkedTypeOff == nestedTypeOff && typeVertex.id == checkedTypeVertex.id &&
                  checkedType2.id == nestedTypeCheck.id && checkedType2Off == nestedTypeCheckOff && typeVertexCheck.id == checkedType2Vertex.id) ||
                  // Check vice versa
                  (checkedType.id == nestedTypeCheck.id && checkedTypeOff == nestedTypeCheckOff && typeVertexCheck.id == checkedTypeVertex.id &&
                    checkedType2.id == nestedType.id && checkedType2Off == nestedTypeOff && typeVertex.id == checkedType2Vertex.id)

            }.isEmpty) {

              // Debug
              DsOliLogger.debug(funSignature + "testing type: " + nestedType.id + " against: " + nestedTypeCheck.id)
              DsOliLogger.debug(funSignature + "\ttype: " + nestedType.id + " instances: " + nestedType.nestedTypeInstances)
              DsOliLogger.debug(funSignature + "\ttype: " + nestedTypeCheck.id + " instances: " + nestedTypeCheck.nestedTypeInstances)

              if (nestedTypeCheck.id == nestedType.id &&
                ((nestedTypeCheck.nestedTypeInstances.size != nestedType.nestedTypeInstances.size) ||
                  // Number of nested types recorded do not match
                  (nestedTypesPresent(nestedType, nestedTypeOff, nestedTypeCheck, nestedTypeCheckOff) == false ||
                    nestedTypesPresent(nestedTypeCheck, nestedTypeCheckOff, nestedType, nestedTypeOff) == false))) {

                // Debug
                DsOliLogger.debug(funSignature + "found different interpretations of type: " + nestedType.id)
                DsOliLogger.debug(funSignature + "\tnestedType: " + nestedType.nestedTypeInstances)
                DsOliLogger.debug(funSignature + "\tnestedTypeCheck: " + nestedTypeCheck.nestedTypeInstances)

                if (!allInterpretationsForType.contains(nestedType.id)) {
                  allInterpretationsForType.put(nestedType.id, new ListBuffer[(DSIType, Long)])
                }
                if (interpretationDoesNotExist(allInterpretationsForType, nestedType.id, nestedType, nestedTypeOff)) {
                  allInterpretationsForType.get(nestedType.id).get.append((nestedType, nestedTypeOff))
                }
                if (interpretationDoesNotExist(allInterpretationsForType, nestedType.id, nestedTypeCheck, nestedTypeCheckOff)) {
                  allInterpretationsForType.get(nestedTypeCheck.id).get.append((nestedTypeCheck, nestedTypeCheckOff))
                }
                checkedTypes.append(
                  (((nestedType, nestedTypeOff), typeVertex),
                    ((nestedTypeCheck, nestedTypeCheckOff), typeVertexCheck)))
              }
            } else {
              // Debug
              DsOliLogger.debug(funSignature + "already checked: nestedType.id = " + nestedType.id + " nestedTypeCheck.id = " + nestedTypeCheck.id)
              DsOliLogger.debug(funSignature + "\tnestedTypeOff = " + nestedTypeOff + " nestedTypeCheckOff = " + nestedTypeCheckOff)
              DsOliLogger.debug(funSignature + "\ttypeVertex.id = " + typeVertex.id + " typeVertexCheck.id = " + typeVertexCheck.id)
            }

        }
    }
  }

  /**
   *  From the calculated interpretations for a type all possible
   *  combinations are created and combined to arrive at the final
   *  type hypotheses.
   *
   *  @param allCombinations store for the final hypotheses
   *  @param allInterpretationsForType the calculated interpretations for a type
   */
  def createAndCombinePossibleTypeCombinations(allCombinations: ListBuffer[ListBuffer[DSIType]],
    allInterpretationsForType: HashMap[Long, ListBuffer[(DSIType, Long)]]): Unit = {
    val funSignature = classSignature + "createAndCombinePossibleTypeCombinations: "
    val possibleTypeCombinationsPerType = new HashMap[Long, ListBuffer[DSIType]]

    // Cycle through the previously calculated interpretations
    allInterpretationsForType.keys.foreach {
      key =>
        println(funSignature + "All interpretations found for type: " + key)
        val interpretationsForType = allInterpretationsForType.get(key).get
        interpretationsForType.foreach {
          typeVariation =>
            val (typeVariationInst, typeVariationOff) = typeVariation
            println(funSignature + "\tvariation: " + typeVariationInst.nestedTypeInstances)
        }

        // Create all possible combinations for one type
        val possibleTypeCombinations = createPossibleTypeCombinations(interpretationsForType)
        possibleTypeCombinationsPerType.put(key, possibleTypeCombinations)
    }

    // Combine all the possible combinations
    val typeIDKeys = possibleTypeCombinationsPerType.keys
    combinePossibleTypeCombinations(possibleTypeCombinationsPerType, typeIDKeys.toList, index = 0, new ListBuffer[DSIType], allCombinations,
      padding = "")

    // Debug
    println(funSignature + "printing all found combinations")
    allCombinations.foreach {
      combinations =>
        println(funSignature + "combinations: " + combinations)
        combinations.foreach {
          combiType =>
            println(funSignature + "\tcombination type: " + combiType)
            combiType.nestedTypeInstances.foreach {
              nestedTypeInstance =>
                println(funSignature + "\t\tnested instance: " + nestedTypeInstance)
            }
        }

    }

    // Filter out duplicates
    allCombinations.foreach {
      combinations =>
        println(funSignature + "combinations: " + combinations)

        combinations.foreach {
          combiType =>
            val foundDuplicates = allCombinations.filter {
              combinationsTest =>
                combinationsTest.exists {
                  combiTest =>
                    // If it is the same type id, skip
                    combiTest.id != combiType.id &&
                      // All primitive types are equal
                      compareAllPrimitiveTypes(combiTest, combiType) &&
                      allNestedTypesEqual(combiTest.nestedTypeInstances, combiType.nestedTypeInstances)
                }
            }
            if (foundDuplicates.size > 0) {

              println(funSignature + "\tcombination type: " + combiType)
              foundDuplicates.foreach {
                foundDuplicate =>

                  println(funSignature + "\tcombination duplicate: " + foundDuplicate)
              }
              throw new Exception(funSignature + "Can believe we got a duplicates")
            }

        }
    }

  }

  /**
   * Checks if all the nested types of the first instance appear in the second and are
   * type compatible regarding the primitive types
   *
   * @param nestedTypeInstancesFirst the first nested type instances
   * @param nestedTypeInstancesSecond the second nested type instances
   * @return Boolean
   */
  def allNestedTypesEqual(nestedTypeInstancesFirst: ListBuffer[(DSIType, Long)], nestedTypeInstancesSecond: ListBuffer[(DSIType, Long)]): Boolean = {

    // Cycle through the first nested instances
    nestedTypeInstancesFirst.forall {
      nestedTypeInstanceFirst =>
        // There must exist a type instance which is compatible
        nestedTypeInstancesSecond.exists {
          nestedTypeInstanceSecond =>
            // Compatible regarding primitive types
            compareAllPrimitiveTypes(nestedTypeInstanceFirst._1, nestedTypeInstanceSecond._1) &&
              nestedTypeInstanceFirst._2 == nestedTypeInstanceSecond._2 &&
              // Recurse on the nested type instances
              allNestedTypesEqual(nestedTypeInstanceFirst._1.nestedTypeInstances, nestedTypeInstanceSecond._1.nestedTypeInstances)
        }
    }

  }

  /**
   * Exchanges the refined nested instances by removing all
   * instances which are too much and adding all missing
   * instances.
   *
   * @param mergedTypeGraphCopy a copy of the merged type graph
   * @param typeHypothesis
   */
  def exchangeRefinedInstances(mergedTypeGraphCopy: DsOliGraph, typeHypothesis: List[DSIType], combination: ListBuffer[DSIType]): Unit = {
    val funSignature = classSignature + "exchangeRefinedInstances: "
    // Exchange the instances, which were refined
    mergedTypeGraphCopy.graph.nodes.iterator.foreach {
      node =>
        val typeVertex = node.value.asInstanceOf[DsOliTypeVertex]

        DsOliLogger.debug(funSignature + "processing typeVertex: " + typeVertex)

        combination.foreach {
          typeCombi =>

            // Always do the nesting first, as the types keep changing
            val nestedTypesList = nestTypes(typeVertex, typeHypothesis)

            DsOliLogger.debug(funSignature + "processing typeCombi: " + typeCombi)

            val typeCombiAllNestedInstances = getNestedInstancesAsList(typeCombi)
            val instancesToExchange = new ListBuffer[(DSIType, Long)]
            nestedTypesList.foreach {
              dsiTypeInstanceTuple =>
                DsOliLogger.debug(funSignature + "\tinspecting type instance: " + dsiTypeInstanceTuple)
                val (dsiType, dsiTypeOffset) = dsiTypeInstanceTuple
                if (dsiType.id == typeCombi.id || typeCombiAllNestedInstances.exists(_._1.id == dsiType.id)) {
                  DsOliLogger.debug(funSignature + "\tfound instance to exchange: " + dsiTypeInstanceTuple)
                  instancesToExchange.append(dsiTypeInstanceTuple)
                }
            }

            DsOliLogger.debug(funSignature + "\texchanging type instances: ")
            instancesToExchange.foreach {
              instanceTuple =>
                val (instType, instOff) = instanceTuple
                DsOliLogger.debug(funSignature + "\texchanging type instance: " + instanceTuple + " nestedTypes: " + instType.nestedTypeInstances)
                // Either use the enclosing OR the nested type. Doesn't matter which nested instance 
                // we choose in case of multiple nested instances of same type, they should be equal
                val exchangeType = if (instType.id == typeCombi.id) typeCombi else typeCombi.nestedTypeInstances.find(_._1.id == instType.id).get._1

                DsOliLogger.debug(funSignature + "\t\twith type: " + typeCombi + " nestedTypes: " + typeCombi.nestedTypeInstances)
                DsOliLogger.debug(funSignature + "\t\twith exchangeType: " + exchangeType + " nestedTypes: " + exchangeType.nestedTypeInstances)

                // Get all nested instances for the type to exchange

                val instTypeAllNestedInstances = getNestedInstancesAsList(instType)
                DsOliLogger.debug(funSignature + "instTypeAllNestedInstances: " + instTypeAllNestedInstances)

                // Remove all instances which are too much
                val instancesToDel = new ListBuffer[(DSIType, Long)]
                DsOliLogger.debug(funSignature + "checking for deletion:")
                instTypeAllNestedInstances.foreach {
                  instTypeNestedInstTuple =>
                    val (instTypeNestedInst, instTypeNestedInstOff) = instTypeNestedInstTuple
                    DsOliLogger.debug(funSignature + "\tinstTypeNestedInstTuple:" + instTypeNestedInstTuple)
                    DsOliLogger.debug(funSignature + "\ttypeCombiAllNestedInstances.exists:")
                    // Do two checks: 
                    // First) check if this instance was not removed already by prior run. Problem here is, 
                    // that we operate on a copy(!) from the nesting done before. This is why we need to 
                    // check against the real dsiTypeInstances once more!
                    // Second) Search for this type in the nested instances of the blue print. Includes the offset check
                    if (typeVertex.dsiTypeInstances.exists(existingTuple => existingTuple._1.id == instTypeNestedInst.id &&
                      existingTuple._2 == instTypeNestedInstOff) && !typeCombiAllNestedInstances.exists {
                      typeCombiNestedInstTuple =>
                        val (typeCombiNestedInst, typeCombiNestedInstOff) = typeCombiNestedInstTuple

                        DsOliLogger.debug(funSignature + "\t\tchecking:" + typeCombiNestedInstTuple)

                        val idMatch = typeCombiNestedInst.id == instTypeNestedInst.id
                        val offsetMatch = instTypeNestedInstOff - instOff == typeCombiNestedInstOff

                        DsOliLogger.debug(funSignature + "\t\tidMatch:" + idMatch)
                        DsOliLogger.debug(funSignature + "\t\toffsetMatch:" + offsetMatch + ", instTypeNestedInstOff (" +
                          instTypeNestedInstOff + ") - instOff(" + instOff + "):" + (instTypeNestedInstOff - instOff) +
                          " typeCombiNestdInstOff: " + typeCombiNestedInstOff)

                        idMatch && offsetMatch

                    }) {
                      DsOliLogger.debug(funSignature + "\t\tadding: " + instTypeNestedInstTuple)
                      val origTuple = typeVertex.dsiTypeInstances.find {
                        instCheck =>

                          DsOliLogger.debug(funSignature + "orig check: instCheck._1.id: " + instCheck._1.id +
                            ", instTypeNestedInst.id: " + instTypeNestedInst.id + " instCheck._2: " + instCheck._2 +
                            ", instTypeNestedInstOff: " + instTypeNestedInstOff)

                          instCheck._1.id == instTypeNestedInst.id && instCheck._2 == instTypeNestedInstOff
                      }.get
                      instancesToDel.append(origTuple)
                    }
                }

                // Add all instances which are missing
                val instancesToAdd = new ListBuffer[(DSIType, Long)]
                DsOliLogger.debug(funSignature + "checking for addition:")
                typeCombiAllNestedInstances.foreach {
                  typeCombiNestedInstTuple =>
                    val (typeCombiNestedInst, typeCombiNestedInstOff) = typeCombiNestedInstTuple

                    DsOliLogger.debug(funSignature + "\ttypeCombiNestedInstTuple:" + typeCombiNestedInstTuple)
                    DsOliLogger.debug(funSignature + "\tinstTypeAllNestedInstances.exists:")

                    if (!instTypeAllNestedInstances.exists {
                      instTypeNestedInstTuple =>

                        val (instTypeNestedInst, instTypeNestedInstOff) = instTypeNestedInstTuple
                        DsOliLogger.debug(funSignature + "\tchecking:" + instTypeNestedInstTuple)

                        val idMatch = instTypeNestedInst.id == typeCombiNestedInst.id
                        val offsetMatch = instTypeNestedInstOff == instOff + typeCombiNestedInstOff

                        DsOliLogger.debug(funSignature + "\t\tidMatch:" + idMatch)
                        DsOliLogger.debug(funSignature + "\t\toffsetMatch:" + offsetMatch + "typeCombiNestedInstOff(" +
                          typeCombiNestedInstOff + ") + instOff(" + instOff + "):" + (typeCombiNestedInstOff + instOff) +
                          " instTypeNestedOff: " + instTypeNestedInstOff)

                        idMatch && offsetMatch
                    }) {
                      val typeTuple = (typeCombiNestedInst, instOff + typeCombiNestedInstOff)
                      DsOliLogger.debug(funSignature + "\t\tadding: " + typeTuple)
                      instancesToAdd.append(typeTuple)
                    }
                }

                // At first remove the instance itself
                DsOliLogger.debug(funSignature + "removing instanceTuple from dsiTypeInstances(size:" +
                  typeVertex.dsiTypeInstances.size + "): " + instanceTuple)

                val origTuple = typeVertex.dsiTypeInstances.find(instCheck => instCheck._1.id == instType.id && instCheck._2 == instOff).get
                typeVertex.dsiTypeInstances -= origTuple

                DsOliLogger.debug(funSignature + "removed instanceTuple from dsiTypeInstances(size:" + typeVertex.dsiTypeInstances.size + ")")

                instancesToDel.foreach {
                  instToDelTuple =>
                    val (instToDel, instToDelOff) = instToDelTuple
                    typeVertex.dsiTypeInstances -= instToDelTuple
                }

                DsOliLogger.debug(funSignature + "removed instancesToDel(size:" + instancesToDel.size +
                  ") resulting in typeVertex.dsiTypeInstances: (size:" + typeVertex.dsiTypeInstances.size +
                  "): " + typeVertex.dsiTypeInstances)

                // Add the replacement instance
                DsOliLogger.debug(funSignature + "adding type back: typeCombi: " + typeCombi + "\n\texchangeType: " + exchangeType)

                if (!typeVertex.dsiTypeInstances.exists(existingTuple => existingTuple._1.id == exchangeType.id && existingTuple._2 == instOff)) {
                  DsOliLogger.debug(funSignature + "adding type back: typeCombi: " + typeCombi + "\n\texchangeType: " + exchangeType)
                  typeVertex.dsiTypeInstances.append((exchangeType.deepCopy, instOff))
                } else {
                  DsOliLogger.debug(funSignature + "skip (already present) adding type back: typeCombi: " + typeCombi + "\n\texchangeType: " + exchangeType)
                }

                DsOliLogger.debug(funSignature + "added typeCombi resulting in typeVertex.dsiTypeInstances: (size:" +
                  typeVertex.dsiTypeInstances.size + "): " + typeVertex.dsiTypeInstances)

                instancesToAdd.foreach {
                  instanceAddTuple =>
                    if (!typeVertex.dsiTypeInstances.exists(existingTuple => existingTuple._1.id == instanceAddTuple._1.id &&
                      existingTuple._2 == instanceAddTuple._2)) {

                      DsOliLogger.debug(funSignature + "\tadding: " + instanceAddTuple)

                      typeVertex.dsiTypeInstances.append(instanceAddTuple)
                    } else {
                      DsOliLogger.debug(funSignature + "\tskipping: " + instanceAddTuple)
                    }
                }
                DsOliLogger.debug(funSignature + "added instancesToAdd(size:" + instancesToAdd.size +
                  ") resulting in typeVertex.dsiTypeInstances: (size:" + typeVertex.dsiTypeInstances.size + "): " + typeVertex.dsiTypeInstances)

            }
        }
    }

  }

  /**
   * Write out the heap and stack files
   *
   * @param mergedTypeGraphCopy a copy of the merged type graph
   * @param typeHypothesis the hypothesis to write
   * @param allWrittenGraphs all graphs written thus far (obsolete)
   * @param i used in the type name
   * @param id obsolete
   * @param parsedHeapFile the original heap file information
   * @param parsedStackFile the original stack file information
   */
  def writeGloballyRefinedTypes(mergedTypeGraphCopy: DsOliGraph, typeHypothesis: List[DSIType], allWrittenGraphs: ListBuffer[DsOliGraph],
    i: Int, id: Int, parsedHeapFile: HashMap[String, String], parsedStackFile: HashMap[String, HashMap[Long, String]]): Unit = {
    val funSignature = classSignature + "writeGloballyRefinedTypes: "

    allWrittenGraphs.append(mergedTypeGraphCopy.deepCopy)

    println(funSignature + "Evaluating type hypothesis: " + typeHypothesis)

    // Have one global lookup, which stores location site merges
    // DsOliTypeVertex id -> list of instances
    val typeInstancesPerVertex = new HashMap[DsOliTypeVertex, ListBuffer[(DSIType, Long)]]

    val fileName = "types_" + i
    val writer = new PrintWriter(new File(DsOliPathUtils.getPath + DsOliPathUtils.getRefinedTypesDir + fileName + ".taint"))
    val stackWriter = new PrintWriter(new File(DsOliPathUtils.getPath + DsOliPathUtils.getRefinedTypesDir + fileName + ".stack"))

    // Keep refinements of the stack structs
    val stackFileMapping = new HashMap[String, HashMap[Long, String]]
    val heapFileMapping = new ListBuffer[String]

    // Create new type based on hypothesis
    // Write all heap types
    mergedTypeGraphCopy.graph.nodes.iterator.foreach {
      node =>
        val typeVertex = node.value.asInstanceOf[DsOliTypeVertex]
        val nestedTypeInstancesAsc = nestTypes(typeVertex, typeHypothesis)
        //writer.println("Testing typeVertex: " + typeVertex)
        // Iterate all elements from the merged type graph which are on the heap
        if (!typeVertex.isStack) {
          println(funSignature + "checking type vertex: " + typeVertex)

          // Keep track of the written heap objects
          heapFileMapping.append(typeVertex.typeID)

          // Maybe the hypothesis has removed all types from this vertex
          if (nestedTypeInstancesAsc.size == 0) {

            // If we have not found any refinements, just dump the type immediately
            //writer.println("Writing typeVertex without refinements: " + typeVertex)
            println(funSignature + " testing function: " + typeVertex.typeID + " isStack: " + typeVertex.isStack)
            if (typeVertex.typeID != "" && typeVertex.typeID != "-") {
              println(funSignature + " heap: " + typeVertex.typeID + " isStack: " + typeVertex.isStack)
              writer.println("Function = " + typeVertex.typeID)

              val dsOliType = typeDB.getTypeObject(typeVertex.vType.vType).get
              //printTypeVertexRec(dsOliType, writer, "")
              printType(dsOliType, writer, "")

            } else {
              println("skip heap Function = " + typeVertex.typeID + ": " + typeVertex.vType)
            }
          } else {
            // Dump the refined type
            writeTypeVertexRefined2(typeVertex, nestedTypeInstancesAsc, "", writer)

          }
          // Stack
        } else {
          val stackBuffer = new StringWriter()
          // write to stack file
          println(funSignature + " stack: " + typeVertex.typeID + " isStack: " + typeVertex.isStack)
          if (nestedTypeInstancesAsc.size != 0) {
            writeTypeVertexRefined2(typeVertex, nestedTypeInstancesAsc, "", stackBuffer, isStack = true)

            // The ID of stack might already be present, as there might exist multiple struct objects on the stack
            if (!stackFileMapping.contains(typeVertex.typeID)) {
              stackFileMapping.put(typeVertex.typeID, new HashMap[Long, String])
            }
            // Store the refinement of the struct at this particular offset
            stackFileMapping.get(typeVertex.typeID).get.put(typeVertex.offsetFromStart, stackBuffer.toString())

          }
        }

    }

    // Debug
    println(funSignature + "*********************")
    printStackMapping(stackFileMapping)
    println(funSignature + "*********************")
    printStackMapping(parsedStackFile)

    // Write the stack diff
    diffStack(parsedStackFile, stackFileMapping, stackWriter)

    // Debug
    println(funSignature + "*********************")
    heapFileMapping.foreach(id => println("written heap object: " + id))
    writer.write("*************************appended elements**********************\n")

    // Write the heap diff
    diffHeap(parsedHeapFile, heapFileMapping, writer)

    writer.close
    stackWriter.close
    println(funSignature + "done processing types: i = " + i)
  }

  /**
   * Global compatibility between nested structs, as sometimes
   * an outer struct might have different nested interpretations.
   * To keep everything consistent the resulting different
   * possibilities are created.
   *
   * @param result all created type hypotheses
   * @param allDSITypes all found DSI types
   * @param mergedTypeGraph the merged type graph
   */
  def compatibleTypesStage3(result: ListBuffer[List[DSIType]], allDSITypes: ListBuffer[DSIType], mergedTypeGraph: DsOliGraph) = {
    val allHypotheses = new ListBuffer[List[DSIType]]
    val funSignature = classSignature + "compatibleTypesStage3: "
    DsOliLogger.debug(funSignature + "entered")

    val parsedHeapFile = parseOriginalHeapFile()
    val parsedStackFile = parseOriginalStackFile()

    // Cycle through each hypothesis and make it globally consistent
    var u = 0
    result.foreach {
      typeHypothesis =>
        // Create some offset
        var i = 2000000 + u * 100
        var origI = 7000000 + u * 100
        val iEnd = i

        DsOliLogger.debug(funSignature + "processing types start: i = " + i)

        val allNestedTypes = new ListBuffer[((DSIType, Long), DsOliTypeVertex)]
        // Be save and operate on copy
        val mergedTypeGraphCopy = mergedTypeGraph.deepCopy
        collectAllNestedTypes(mergedTypeGraphCopy, typeHypothesis, allNestedTypes)

        // Debug
        DsOliLogger.debug(funSignature + "allNestedTypes.size: " + allNestedTypes.size + " allNestedTypes: ")
        allNestedTypes.foreach(nType => DsOliLogger.debug("\ttype(@" + nType._1._2 + ", typeVertex.id: " + nType._2.id + "): " + nType))

        DsOliLogger.debug(funSignature + "allNestedTypes.size: " + allNestedTypes.size + " allNestedTypes: ")
        allNestedTypes.foreach(nType => DsOliLogger.debug("\ttype(@" + nType._1._2 + ", typeVertex.id: " + nType._2.id + "): " + nType))

        val allInterpretationsForType = new HashMap[Long, ListBuffer[(DSIType, Long)]]
        ensureConsistencyAmongNestedTypes(allNestedTypes, allInterpretationsForType)

        // If there are not refinements, just write the hypothesis immediately
        if (allInterpretationsForType.size == 0) {
          writeOneHypothesisToFile(typeHypothesis, mergedTypeGraph, origI)
        } else {

          // Create all possible combinations of these type combinations
          val allCombinations = new ListBuffer[ListBuffer[DSIType]]
          createAndCombinePossibleTypeCombinations(allCombinations, allInterpretationsForType)

          val allWrittenGraphs = new ListBuffer[DsOliGraph]

          // Iterate over all globally consistent type combinations
          allCombinations.foreach {
            combination =>

              DsOliLogger.debug(funSignature + "processing types: i = " + i)
              DsOliLogger.debug(funSignature + "refining typeHypothesis: " + typeHypothesis + " with combination: " + combination)

              // Write the original type
              val id = 7000000 + u * 100
              //writeRefinedTypesToFile(result, mergedTypeGraph, id)

              // Be save and operate on copy
              val mergedTypeGraphCopy = mergedTypeGraph.deepCopy

              exchangeRefinedInstances(mergedTypeGraphCopy, typeHypothesis, combination)

              writeGloballyRefinedTypes(mergedTypeGraphCopy, typeHypothesis, allWrittenGraphs, i, id, parsedHeapFile, parsedStackFile)

              i += 1
          }
        }

        u += 1
        DsOliLogger.debug(funSignature + "processing types end: i = " + iEnd)
    }

    // Currently stop after the refinement process has finished
    System.exit(0)

    allHypotheses
  }

  /**
   * Recursively collect all combinations for each type forming
   * the final hypotheses
   *
   * @param possibleTypeCombinationsPerType the calculated combinations per type
   * @param typeIDKeys all types found
   * @param index the current index into the typeIDKeys (incremented with each recursion)
   * @param curCombinations the current set of calculated combinations
   * @param allCombinations stores the resulting hypotheses by storing a copy of the current combination as recursion base case
   */
  def combinePossibleTypeCombinations(possibleTypeCombinationsPerType: HashMap[Long, ListBuffer[DSIType]],
    typeIDKeys: List[Long], index: Int, curCombinations: ListBuffer[DSIType], allCombinations: ListBuffer[ListBuffer[DSIType]],
    padding: String): Unit = {
    val funSignature = padding + classSignature + "combinePossibleTypeCombinations: "
    DsOliLogger.debug(funSignature + "entered: index: " + index + " curCombinations: " + curCombinations)

    // If last element, store the result and return
    if (index >= typeIDKeys.size) {
      val curCombinationsClone = curCombinations.clone
      DsOliLogger.debug(funSignature + "curCombinationsClone: " + curCombinationsClone)
      allCombinations.append(curCombinationsClone)
      return
    }

    // Fetch the type to consider indicated by current index
    val typeIDKey = typeIDKeys(index)
    // Fetch all the combinations for this type
    val typeCombinations = possibleTypeCombinationsPerType.get(typeIDKey).get
    // Cycle through each combination and recurse
    typeCombinations.foreach {
      typeCombination =>

        // Add current element
        DsOliLogger.debug(funSignature + "adding combination: " + typeCombination)
        curCombinations.append(typeCombination)

        DsOliLogger.debug(funSignature + "recurse")
        // Recurse
        combinePossibleTypeCombinations(possibleTypeCombinationsPerType, typeIDKeys, index + 1, curCombinations, allCombinations,
          padding + " ")

        // Remove last element
        DsOliLogger.debug(funSignature + "removing last combination: curCombinations.size: " + curCombinations.size)
        curCombinations.remove(curCombinations.size - 1)
        DsOliLogger.debug(funSignature + "removed last combination: curCombinations.size: " + curCombinations.size)
    }

  }

  /**
   * Calculate the combinations of the nested types. It works
   * by fetching all nested types and then doing the combinatorial
   * part by representing the nested types as an Long, where each
   * position (bit) in the Long represents if the nested type
   * is used or not. Thus incrementing the Long from no bits set
   * to all bits set produces all possible nested type combinations.
   * Compatibility of all nested types is also checked.
   *
   * @param interpretationsForType the interpretations found for a type
   * @return list of type combinations
   *
   */
  def createPossibleTypeCombinations(interpretationsForType: ListBuffer[(DSIType, Long)]): ListBuffer[DSIType] = {
    val funSignature = classSignature + "createPossibleTypeCombinations: "
    val typeCombinations = new ListBuffer[DSIType]

    // Store the absolute offset for this particular type instance.
    // This is needed, as the rest works with relative offsets of the nested elements
    val absoluteOffsetEnclosing = interpretationsForType.head._2

    // Always create a deep copy of every type

    // Create an empty default type without any nested types
    val emptyType = interpretationsForType.head._1.deepCopy
    emptyType.nestedTypeInstances.clear

    // Now do the combinatorial part
    val allNestedTypeInstances = new ListBuffer[(DSIType, Long)]
    // Cycle through all interpretations for this type
    interpretationsForType.foreach {
      interpretationTuple =>
        val (interpretation, interpretationOff) = interpretationTuple
        // Cycle through all the specific nested types for this instance
        DsOliLogger.debug(funSignature + "combinatorial checking interpretation: " + interpretation.id)
        val nestedTypeInstances = getNestedInstancesAsList(interpretation)
        nestedTypeInstances.foreach {
          nestedInstanceTuple =>
            val (nestedTypeInst, nestedTypeInstOff) = nestedInstanceTuple
            DsOliLogger.debug(funSignature + "combinatorial checking nested instance: " + nestedTypeInst.id)
            if (!allNestedTypeInstances.exists {
              nestedTypeInstance =>
                val (allNestedTypeInst, allNestedTypeInstOff) = nestedTypeInstance
                DsOliLogger.debug(funSignature + "testing instance from allNestedTypeInst: " + allNestedTypeInst.id)
                // Shortcut test on id
                val idMatch = allNestedTypeInst.id == nestedTypeInst.id
                // Shortcut test on nested instances size
                val sizeMatch = allNestedTypeInst.nestedTypeInstances.size == nestedTypeInst.nestedTypeInstances.size
                // If nested instances sizes match, this does not mean, that all elements from 
                // first type are actually present in second type (and vice versa)
                val nestingsMatch = nestedTypesPresent(allNestedTypeInst, allNestedTypeInstOff, nestedTypeInst, nestedTypeInstOff) &&
                  nestedTypesPresent(nestedTypeInst, nestedTypeInstOff, allNestedTypeInst, allNestedTypeInstOff)
                DsOliLogger.debug(funSignature + "\tidMatch: " + idMatch)
                DsOliLogger.debug(funSignature + "\tsizeMatch: " + sizeMatch)
                DsOliLogger.debug(funSignature + "\tnestingsMatch: " + nestingsMatch)

                idMatch && sizeMatch && nestingsMatch

            }) {
              DsOliLogger.debug(funSignature + "found orig instance: " + nestedInstanceTuple)
              // Calculate the nested offset RELATIVE to the enclosing type
              val nestedOffsetRelativeFromEnclosing = (nestedTypeInstOff - interpretationOff)
              DsOliLogger.debug(funSignature + "new offset for found orig instance: " + nestedOffsetRelativeFromEnclosing)
              allNestedTypeInstances.append((nestedTypeInst, nestedOffsetRelativeFromEnclosing))
            }
        }
    }

    // Attention: if the size of the nested instances is greater than the number of bits of a Long
    if (allNestedTypeInstances.size > 64) throw new Exception(funSignature + "number of supported nested types exceeded!")

    // Now cycle through all the possible combinations
    val numberOfCombinations = Math.pow(2, allNestedTypeInstances.size).toLong
    DsOliLogger.debug(funSignature + "numberOfCombinations: " + numberOfCombinations + " allNestedTypeInstances.size: " + allNestedTypeInstances.size)
    for (i <- 0L until numberOfCombinations) {
      DsOliLogger.debug(funSignature + "creating combination: " + i)
      val combinationsToCompare = new ListBuffer[(DSIType, Long)]
      // Only iterate at most number of instances: this creates the combinations
      // of the nested instances => if the bit is set (1) the instance is part of the combination
      for (index <- 0 until allNestedTypeInstances.size) {
        DsOliLogger.debug(funSignature + "\ttesting bit at position: " + index)
        val curBit = (i >>> index) & 0x1
        if (curBit == 1) {
          DsOliLogger.debug(funSignature + "\tbit is set")
          combinationsToCompare.append(allNestedTypeInstances(index))
        } else {
          DsOliLogger.debug(funSignature + "\tbit is not set")
        }
      }

      // Now do a pairwise comparison of all the instances. They all need to be 
      // compatible, otherwise the combination will be discarded
      val combinationsToTest = combinations(combinationsToCompare.toList)
      val allCompatible = combinationsToTest.forall {
        combinationTuple =>
          val ((firstType, firstOffset), (secondType, secondOffset)) = combinationTuple
          // The offset is stored relative to enclosing, but it is not required to calculate absolute offset again as
          // the offsets should reflect the correct nesting relative to the enclosing type they are nested in
          !isOverlapping(firstType, firstOffset, secondType, secondOffset) || isNested(firstType, firstOffset, secondType, secondOffset)
      }

      // Record the result, if types are compatible
      if (allCompatible) {
        DsOliLogger.debug(funSignature + "type instances are compatible:")
        val newType = emptyType.deepCopy
        combinationsToCompare.foreach(instance => newType.nestedTypeInstances.append((instance._1.deepCopy, instance._2)))
        typeCombinations.append(newType)
      } else {
        DsOliLogger.debug(funSignature + "type instances are not compatible:")
      }

      combinationsToCompare.foreach {
        instance =>
          DsOliLogger.debug(funSignature + "instance: " + instance)
      }
    }

    DsOliLogger.debug(funSignature + "printing all type combinations:")
    typeCombinations.foreach {
      typeCombination =>
        DsOliLogger.debug(funSignature + "type: " + typeCombination.id)
        typeCombination.nestedTypeInstances.foreach {
          nestedTypeInstance =>
            DsOliLogger.debug(funSignature + "\tnested type instance: " + nestedTypeInstance)
        }
    }

    typeCombinations

  }

  /**
   * Calculate all possible type combinations thus creating the actual
   * type hypotheses. Additionally write out the results.
   *
   * @param mergedTypeGraph the merged type graph
   * @param allDSITypes all collected DSI types
   */
  def calculateTypeCombinations(mergedTypeGraph: DsOliGraph, allDSITypes: ListBuffer[DSIType]) = {
    val funSignature = classSignature + "calculateTypeCombinations: "
    DsOliLogger.debug(funSignature + "entered")

    // First check the local compatibility (i.e., within one vertex) for the types
    compatibleTypesStage1(mergedTypeGraph)

    // Now check the global compatibility between types, resulting in a global lookup
    // for type compatibility: the global type matrix
    val globalTypeMatrix = compatibleTypesStage2(mergedTypeGraph, allDSITypes)

    // Now create the hypothesis
    val result = calculateAllHypothesis(allDSITypes, globalTypeMatrix)

    //throw new Exception("result: " + result.size)

    DsOliLogger.debug(funSignature + "dsi types per type vertex: ")
    mergedTypeGraph.graph.nodes.iterator.foreach {
      node =>
        val typeVertex = node.value.asInstanceOf[DsOliTypeVertex]
        DsOliLogger.debug(funSignature + "typeVertex: " + typeVertex.vType.vType)
        typeVertex.dsiTypeInstances.foreach {
          instance =>
            DsOliLogger.debug(funSignature + "type (@off:" + instance._2 + "):" + instance._1)
        }
    }

    // Create multiple merged typeGraphs which are all compatible
    // regarding the (nested) DSIType instances
    val unifiedMergedTypeGraphs = compatibleTypesStage3(result, allDSITypes, mergedTypeGraph)

  }

  /**
   * Select the type instances inside of a type vertex for a given hypothesis
   *
   * @param typeVertex the type vertex to check
   * @param types the hypothesis
   * @return list of type instances of the given hypothesis present in the current type vertex
   */
  def selectTypeInstancesInVertex(typeVertex: DsOliTypeVertex, types: List[DSIType]): ListBuffer[(DSIType, Long)] = {
    val funSignature = classSignature + "selectTypeInstancesInVertex: "
    DsOliLogger.debug(funSignature + "entered: " + typeVertex)
    val typeInstancesPerVertex = new ListBuffer[(DSIType, Long)]

    // Check through the types of the hypothesis
    types.foreach {
      fetchType =>

        if (fetchType != null) {

          // Filter for instances of this type
          val foundTypeInstances = typeVertex.dsiTypeInstances.filter {
            typeInstance =>
              val (compType, compOffset) = typeInstance
              compType.id == fetchType.id
          }

          // Store, if instances are found
          if (foundTypeInstances.size > 0) {
            DsOliLogger.debug(funSignature + "found: " + fetchType)
            // Important! Create a new object of this type, to always operate on a fresh copy of nested types
            foundTypeInstances.foreach {
              foundTypeInstance =>
                DsOliLogger.debug(funSignature + "appending instance (@off: " + foundTypeInstance._2 + "): " + foundTypeInstance._1)
                typeInstancesPerVertex.append((foundTypeInstance._1.deepCopy, foundTypeInstance._2))
            }
          } else {
            DsOliLogger.debug(funSignature + "not found: " + fetchType)
          }
        } else {
          DsOliLogger.debug(funSignature + "typeVertex does not have type instances: " + fetchType)
        }
    }
    typeInstancesPerVertex
  }

  /**
   * Nest all DSI type instances in a type vertex.
   * The nested elements are always directly stored inside
   * of their immediate parent. So one needs to follow
   * multiple nesting levels to fetch all nested elements.
   *
   * @param typeVertex the top most vertex which will also store the nested elements
   * @param hypothesis the current type hypothesis
   * @return a list of all nested instances, as is, without nesting property
   */
  def nestTypes(typeVertex: DsOliTypeVertex, hypothesis: List[DSIType]): ListBuffer[(DSIType, Long)] = {
    val funSignature = classSignature + "nestTypes: "
    DsOliLogger.debug(funSignature + "entered: " + typeVertex)
    val nestedTypesPerVertex = new ListBuffer[(DSIType, Long)]
    val typeInstancesPerVertex = selectTypeInstancesInVertex(typeVertex, hypothesis)
    val typeInstancesPerVertexAsc = typeInstancesPerVertex.sortBy(_._1.byteSize)

    // Calculate the nesting: try to nest the prev type with the cur type 
    for (i <- 0 until typeInstancesPerVertexAsc.length) {
      val (dsiTypePrev, offsetPrev) = typeInstancesPerVertexAsc(i)
      DsOliLogger.debug(funSignature + "dsiTypePrev: " + dsiTypePrev.id + " byte size: " + dsiTypePrev.byteSize)
      breakable {
        for (u <- i + 1 until typeInstancesPerVertexAsc.length) {
          val (dsiTypeCur, offsetCur) = typeInstancesPerVertexAsc(u)
          DsOliLogger.debug(funSignature + "dsiTypeCur: " + dsiTypeCur.id + " byte size: " + dsiTypeCur.byteSize)
          // Skip types which are equal
          if (dsiTypePrev.byteSize != dsiTypeCur.byteSize && isNested(dsiTypePrev, offsetPrev, dsiTypeCur, offsetCur)) {
            DsOliLogger.debug(funSignature + "dsiTypePrev(@" + offsetPrev + ", " + dsiTypePrev.byteSize + "): " + dsiTypePrev.id + " is nested in dsiTypeCur(@" + offsetCur + ", " + dsiTypeCur.byteSize + "): " + dsiTypeCur.id)
            dsiTypePrev.isNested = true
            dsiTypeCur.nestedTypeInstances.append((dsiTypePrev, offsetPrev))
            break
          }
        }
      }
    }

    // Debug
    DsOliLogger.debug(funSignature + "all nested types for vertex: " + typeVertex)
    typeInstancesPerVertexAsc.foreach {
      typeInstance =>
        val (dsiType, offset) = typeInstance
        DsOliLogger.debug(funSignature + "type instance (is nested: " + dsiType.isNested + "): " + dsiType)
        if (dsiType.isNested == false && dsiType.nestedTypeInstances.size > 0) {

          dsiType.nestedTypeInstances.foreach {
            nestedTypeInstance =>
              val (nestedDsiType, nestedOffset) = nestedTypeInstance
              DsOliLogger.debug(funSignature + "\tnested type(" + nestedOffset + "): " + nestedDsiType)
          }
        }
    }

    typeInstancesPerVertexAsc
  }

  /**
   * Calculate type refinements for Howard types, based on the
   * merged type graph built during program execution.
   *
   * @param mergedTypeGraph the merged type graph
   */
  def calculateDSITypes(mergedTypeGraph: DsOliGraph) = {

    val funSignature = classSignature + "calculateDSITypes: "
    DsOliLogger.debug(funSignature + " entered")

    val allDSITypes = new ListBuffer[DSIType]
    // Process each edge of the merged type graph
    //mergedTypeGraph.graph.edges.iterator.foreach {
    mergedTypeGraph.graph.edges.toList.sortBy(edge => edge.toOuter.id).iterator.foreach {
      edgeInner =>
        //edge =>
        //DsOliLogger.debug(funSignature + " testing edge (" + edge.source.value.id + ":" + edge.sOffset + "->" +
        // edge.target.value.id + ":" + edge.tOffset + ")" +
        val edge = edgeInner.toOuter
        DsOliLogger.debug(funSignature + " testing edge (" + edge.source.id + ":" + edge.sOffset + "->" +
          edge.target.id + ":" + edge.tOffset + ")" +
          ": " + edge)
        // Calculate all possible DSI types by inspecting one edge
        //val dsiTypes = calculateDSITypesForEdge(edge.toOuter)
        val dsiTypes = calculateDSITypesForEdge(edge)

        println(dsiTypes)
        //throw new Exception("stop")

        // Now try to propagate these types maximally on the edges
        // in the merged type graph
        dsiTypes.foreach {
          dsiTypeTuple =>
            val (dsiType, sourceOffset) = dsiTypeTuple
            DsOliLogger.debug(funSignature + " Found DSI type (id: " + dsiType.id + " source offset: " + sourceOffset +
              ", linkageOffset: " + dsiType.linkageOffset + "): " + dsiType.primitiveTypes)
            //propagateDSITypeOnMaximalPath(dsiType, sourceOffset, edge.source.value, mergedTypeGraph)
            propagateDSITypeOnMaximalPath(dsiType, sourceOffset, edge.source, mergedTypeGraph)

            // Keep track of each type
            allDSITypes.append(dsiType)
        }

    }

    // Debug
    DsOliLogger.debug(funSignature + "all found dsi types: ")
    allDSITypes.foreach {
      dsiType =>
        DsOliLogger.debug(funSignature + "dsi type: id: " + dsiType.id + ", group id: " + dsiType.groupId + " linkageOffset: " +
          dsiType.linkageOffset + " primitiveTypes:" + dsiType.primitiveTypes)
    }

    DsOliLogger.debug(funSignature + "dsi types per type vertex: ")
    mergedTypeGraph.graph.nodes.iterator.foreach {
      node =>
        val typeVertex = node.value.asInstanceOf[DsOliTypeVertex]
        DsOliLogger.debug(funSignature + "typeVertex: " + typeVertex.vType.vType)
        typeVertex.dsiTypeInstances.foreach {
          instance =>
            DsOliLogger.debug(funSignature + "type (@off:" + instance._2 + "):" + instance._1)
        }
    }

    // Now calculate all possible type combinations. This also includes writing the
    // results out in the end.
    calculateTypeCombinations(mergedTypeGraph, allDSITypes)
  }

}