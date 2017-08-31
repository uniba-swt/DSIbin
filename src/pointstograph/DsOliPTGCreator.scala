
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
 * DsOliPTGCreator.scala created on Oct 15, 2014
 *
 * Description: The points-to graph (PTG) creator component. It creates the PTGs
 * for each time step of the event trace
 */
package pointstograph

import event.DsOliEvents
import event.DsOliMemoryEvent
import event.DsOliMWEvent
import scalax.collection.mutable.Graph
import scala.util.control.Breaks._
import scala.collection.mutable.Set
import pointstograph.DsOliDiEdge._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import extlogger._
import pointstograph.FieldType._
import event.DsOliVLSEvent
import event.DsOliVLSEvent
import test.DsOliTestMethods
import event.DsOliFreeEvent
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap
import event.DsOliFreeEvent
import event.DsOliVLSEvent
import event.DsOliMWEvent
import util.DsOliAddressUtils
import event.DsOliMemoryEvent
import event.DsOliFreeEvent
import event.DsOliVLSEvent
import event.DsOliEvent
import event.DsOliArtificialUndefEvent
import event.DsOliArtificialFreeEvent
import event.DsOliArtificialFreeEvent
import event.DsOliMWEvent
import event.DsOliNoAllocEvent
import event.DsOliLValue
import event.DsOliRValue
import PartialFunction._
import event.DsOliNoAllocEvent
import event.DsOliVESEvent
import event.DsOliMallocEvent
import event.DsOliMallocEvent
import PartialFunction._
import binarytypes.DSIType

trait IDsOliPTGCreator {
  def createPTGs(): (DsOliPointsToGraphs, DsOliGraph)
}

/**
 * @author DSI
 *
 */
/**
 * @constructor create a PTG creator
 * @param events the event trace
 * @param typeDB the type DB
 */
class DsOliPTGCreator(val events: DsOliEvents, val typeDB: ITypeDB) extends IDsOliPTGCreator {
  val classSignature = "DsOliPTGCreator::"

  // ID for vertices: starts with 2 because of predefined NULL/undef vertices
  var id = 2
  /**
   * Unique id generator for vertices
   *
   * @returns a unique vertex id
   */
  def getId(): Long = {
    val funSignature = classSignature + "getId: "
    DsOliLogger.debug(funSignature + "entered: ")
    val tmpId = id
    id += 1
    return tmpId
  }

  /**
   * Setup the initial PTG which only contains the NULL
   * and undefined vertex
   *
   * @param ptg the store for all subsequent PTGs
   * @return The initial points-to graph
   */
  def createInitialPTG(ptg: DsOliPointsToGraphs): DsOliGraph = {
    val funSignature = classSignature + "createInitialPTG: "
    DsOliLogger.debug(funSignature + "entered: ")
    // Keep a reference to the store of all PTGs
    val initialPTG = new DsOliEventGraph(ptg)
    initialPTG.add(DsOliGraph.vertexNull)
    initialPTG.add(DsOliGraph.vertexUndef)
    return initialPTG
  }

  /**
   * Checks, if the event has a context, i.e., direct pointer assignment or not
   *
   * @param event the event to check
   * @return Boolean
   */
  def hasContext(event: DsOliMWEvent): Boolean = {
    val funSignature = classSignature + "hasContext: "
    DsOliLogger.debug(funSignature + "entered: ")
    // exactly one member means, that we are directly assigning to a pointer
    // => more members means context
    return event.lValueList.length > 1
  }

  /**
   * Checks if address A is in range of address B
   *
   * @param vertexAStartAddress start address of vertex A
   * @param vertexAEndAddress end address of vertex A
   * @param vertexBStartAddress start address of vertex B
   * @param vertexBEndAddress end address of vertex B
   * @returns Boolean
   */
  def addressInRange(vertexAStartAddress: Long, vertexAEndAddress: Long, vertexBStartAddress: Long, vertexBEndAddress: Long): Boolean = {
    val funSignature = classSignature + "addressInRange: "
    DsOliLogger.debug(funSignature + "entered: ")
    // following condition is assumed: vertexBStartAddress < vertexBEndAddress!
    return vertexAStartAddress <= vertexBStartAddress && vertexBEndAddress <= vertexAEndAddress
  }

  /**
   * Find a vertex within the given start/end address
   *
   * @param nodes the vertices of the graph
   * @param vertexStartAddress the start address of the vertex
   * @param vertexEndAddress the end address of the vertex
   * @returns Option the vertex in range
   */
  def findVertexInRange(nodes: Graph[pointstograph.DsOliVertex, pointstograph.DsOliDiEdge]#NodeSetT, vertexStartAddress: Long, vertexEndAddress: Long): Option[DsOliVertex] = {
    val funSignature = classSignature + "findVertexInRange: "
    DsOliLogger.debug(funSignature + "entered: ")
    for (node <- nodes.iterator) {
      val vertex = node.value
      condOpt(vertex) {
        case v: DsOliVertexMemory =>
          if (addressInRange(v.bAddr, v.eAddr, vertexStartAddress, vertexEndAddress)) {
            return Some(v)
          }
      }
    }
    return None
  }

  /**
   * Convenience method to create a new memory vertex
   *
   * @param vertexType the type associated with the vertex
   * @param vertexStartAddress the start address of the vertex
   * @param vertexEndAddress the end address of the vertex
   * @param isStack indicates stack or heap
   * @return a newly created memory vertex
   */
  def createNewVertex(vertexType: DsOliType, vertexStartAddress: Long, vertexEndAddress: Long, isStack: Boolean): DsOliVertexMemory = {
    val funSignature = classSignature + "createNewVertex: "
    DsOliLogger.debug(funSignature + "entered: " + vertexType + " isStack: " + isStack)
    return new DsOliVertexMemory(vertexStartAddress, vertexEndAddress, vertexType, getId(), isStack)
  }

  /**
   * Convenience method to create a new type vertex
   *
   * @param vertexType the type associated with the vertex
   * @param vertexStartAddress the start address of the vertex
   * @param vertexEndAddress the end address of the vertex
   * @param isStack indicates stack or heap
   * @param typeID id of the type generated by instrumentation framework
   * @param offsetFromStart
   * @return a newly created memory vertex
   */
  def createNewTypeVertex(vertexType: DsOliType, vertexStartAddress: Long, vertexEndAddress: Long, isStack: Boolean, typeID: String, offsetFromStart: Long): DsOliVertexMemory = {
    val funSignature = classSignature + "createNewTypeVertex: "
    DsOliLogger.debug(funSignature + "entered: " + vertexType + " isStack: " + isStack)
    return new DsOliTypeVertex(vertexStartAddress, vertexEndAddress, vertexType, getId(), isStack, typeID, offsetFromStart, new ListBuffer[(DSIType, Long)])
  }

  /**
   * Calculate the offset for a resized vertex
   *
   * @param newVertex the newly created/resized vertex
   * @param oldStartAddr the start address of the old vertex
   * @param oldOffset the offset within the old vertex
   * @param the new offset
   */
  def calculateResizedOffset(newVertex: DsOliVertexMemory, oldStartAddr: Long, oldOffset: Long): Long = {
    val funSignature = classSignature + "calculateResizedOffset: "
    DsOliLogger.debug(funSignature + "entered: ")
    return ((oldStartAddr + oldOffset) - newVertex.bAddr)
  }

  /**
   * Re-calculate vertices and in/out going edges in case of a vertex resize
   *
   * @param newVertex the new resized vertex
   * @param graph the current points-to graph
   */
  def cleanupEdgesAndVertices(newVertex: DsOliVertexMemory, graph: DsOliGraph): Unit = {
    val funSignature = classSignature + "cleanupEdgesAndVertices: "
    DsOliLogger.debug(funSignature + "entered: ")
    val vertices = graph.graph.nodes
    val verticesInRange = Set[DsOliVertex]()
    val newIncomingEdges = Set[DsOliDiEdge[DsOliVertex]]()
    val newOutgoingEdges = Set[DsOliDiEdge[DsOliVertex]]()
    for (node <- vertices.iterator) {
      val vertex = node.value
      condOpt(vertex) {
        case v: DsOliVertexMemory =>
          if (addressInRange(newVertex.bAddr, newVertex.eAddr, v.bAddr, v.eAddr)) {
            verticesInRange.add(v)

            // Handle incoming
            val deleteEdges = Set[DsOliDiEdge[DsOliVertex]]()

            val inRangeVertex = graph.graph.get(v)
            inRangeVertex.incoming.foreach {
              edge =>
                val oldEdge = edge.toEdgeIn
                val sourceVertex = edge.source.value
                val oldTargetVertex = edge.target.value
                newIncomingEdges.add(sourceVertex ~> newVertex toDsOliDiEdge (oldEdge.sAddr, oldEdge.sOffset,
                  newVertex.bAddr, calculateResizedOffset(newVertex, oldEdge.tAddr, oldEdge.tOffset)))
                deleteEdges.add(oldEdge)
            }

            deleteEdges.foreach {
              edge =>
                DsOliLogger.debug(funSignature + "deleting incoming edge: " + edge)
                graph.graph -= edge
            }

            // Handle outgoing
            deleteEdges.clear

            inRangeVertex.outgoing.foreach {
              edge =>
                val oldEdge = edge.toEdgeIn
                val sourceVertex = edge.source.value //edge.edge.source.value
                val oldTargetVertex = edge.target.value
                newOutgoingEdges.add(newVertex ~> oldTargetVertex toDsOliDiEdge (newVertex.bAddr,
                  calculateResizedOffset(newVertex, oldEdge.sAddr, oldEdge.sOffset),
                  oldEdge.tAddr, oldEdge.tOffset))
                deleteEdges.add(oldEdge)
            }

            deleteEdges.foreach {
              edge =>
                DsOliLogger.debug(funSignature + "deleting outgoing edge: " + edge)
                graph.graph -= edge
            }

          }
      }
    }

    // Delete vertices
    verticesInRange.foreach {
      vertex =>
        DsOliLogger.debug(funSignature + "deleting vertex: " + vertex)
        graph.graph -= vertex
    }

    DsOliLogger.debug(funSignature + "adding new vertex: " + newVertex)
    // Timing is important here. The new vertex
    // needs to be added after the vertices have
    // been processed. Otherwise the vertex would
    // be removed by the above algorithm
    graph.add(newVertex)

    // Add the newly created in/out going edge
    newIncomingEdges.foreach {
      //graph.graph += _
      graph.add(_)
    }

    newOutgoingEdges.foreach {
      //graph.graph += _
      graph.add(_)
    }

    val innerNewVertex = graph.graph.get(newVertex)
    // Handle the initialization to UNDEF of uninitialized fields
    // Note: Use of type information
    if (newVertex.vType != null && newVertex.vType.fields != null) {
      newVertex.vType.fields.foreach {
        field =>
          if (field.fType == Pointer && !innerNewVertex.outgoing.exists {
            edge =>
              edge.edge.sOffset == field.vOffset
          }) {
            addNewEdge(newVertex, field.vOffset, DsOliGraph.vertexUndef, 0, graph)
          }
      }
    }

  }

  /**
   * Try to find a vertex in range for a given start address and type (size)
   *
   * @param vertexStartAddress start address of the vertex
   * @param vertexType the type associated with the vertex
   * @param graph the current points-to graph
   * @param size the size of the vertex
   * @param isStack indicates if the type is on the stack or not
   * @return Either a newly created vertex or vertex found in the current points-to graph
   */
  def findOrAddVertex(vertexStartAddress: Long, vertexType: DsOliType, graph: DsOliGraph, size: Long, isStack: Boolean = false): DsOliVertex = {
    val funSignature = classSignature + "findOrAddVertex: "
    DsOliLogger.debug(funSignature + "entered. vertexStartAddress: " +
      vertexStartAddress.toHexString + ", vertexType: " + vertexType)
    val vertices = graph.graph.nodes

    val vertexEndAddress = vertexStartAddress + size - 1

    // Try to find a vertex in range. If not found, create new
    return findVertexInRange(vertices, vertexStartAddress, vertexStartAddress) match {
      case Some(v) =>
        DsOliLogger.debug(funSignature + "found vertex in range: vertexStartAddress: " +
          vertexStartAddress.toHexString + " vertexEndAddress: " +
          vertexEndAddress.toHexString + " size: " + size + ": " + v)
        v
      case None =>
        val newVertex = createNewVertex(vertexType, vertexStartAddress, vertexEndAddress, isStack)
        DsOliLogger.debug(funSignature + "created new vertex: " + newVertex)
        cleanupEdgesAndVertices(newVertex, graph)
        newVertex
    }

  }

  /**
   * Remove an edge originating at the given source address
   *
   * @param event the memory write event
   * @param sourceVertex the source vertex where the edge originates
   * @param sourceAddress the source address where the edge originates
   * @param graph the current points-to graph
   */
  def removeOldEdge(event: DsOliMWEvent, sourceVertex: DsOliVertex, sourceAddress: Long, graph: DsOliGraph): Unit = {
    val funSignature = classSignature + "removeOldEdge: "
    DsOliLogger.debug(funSignature + "entered: sourceVertex: " + sourceVertex + ", sourceAddress: " + sourceAddress)
    val innerSourceVertex = graph.graph.get(sourceVertex)
    val outgoingEdge = innerSourceVertex.outgoing.filter {
      edgeIter =>
        DsOliLogger.debug(funSignature + "Testing edge: " + edgeIter +
          " sourceAddress: " + sourceAddress + " (edgeIter.edge.sAddr + edgeIter.edge.sOffset): " +
          (edgeIter.edge.sAddr + edgeIter.edge.sOffset))
        edgeIter.edge.sAddr + edgeIter.edge.sOffset == sourceAddress
    }
    // There should be only one edge originating from the given source address
    if (outgoingEdge.size == 1) {
      graph.graph -= outgoingEdge.head
    } else if (outgoingEdge.size > 1) {
      DsOliLogger.warning(funSignature + "Can't remove the edge, as there are multiple ones: " + sourceVertex)
      outgoingEdge.foreach {
        edge =>
          DsOliLogger.warning("\t edge: " + edge)
      }
    } else {
      DsOliLogger.debug(funSignature + "no edge for vertex: " + sourceVertex)
    }
  }

  /**
   * Create a new edge
   *
   * @param sourceVertex the vertex where the edge originates from
   * @param sourceOffset the offset within the source vertex where the edge originates from
   * @param targetOffset the offset within the target vertex where the edge originates from
   * @param targetOffset the offset within the target vertex where the edge originates from
   * @param graph the current points-to graph
   */
  def addNewEdge(sourceVertex: DsOliVertexMemory, sourceOffset: Long, targetVertex: DsOliVertex, targetOffset: Long, graph: DsOliGraph): Unit = {
    val funSignature = classSignature + "addNewEdge: "
    DsOliLogger.debug(funSignature + "entered: ")
    targetVertex match {
      case tv: DsOliVertexMemory =>
        DsOliLogger.debug(funSignature + "DsOliVertexMemory.")
        graph.add(sourceVertex ~> targetVertex toDsOliDiEdge (sourceVertex.bAddr, sourceOffset, tv.bAddr, targetOffset))

      case tv: DsOliVertexPredefined =>
        DsOliLogger.debug(funSignature + "DsOliVertexNull.")
        graph.add(sourceVertex ~> tv toDsOliDiEdge (sourceVertex.bAddr, sourceOffset, 0, 0))

      case _ =>
        throw new Exception("Vertex currently not supported: " + targetVertex)
    }
  }

  /**
   * Create a new type edge for the merged type graph
   *
   * @param sourceVertex the vertex where the edge originates from
   * @param sourceOffset the offset within the source vertex where the edge originates from
   * @param targetOffset the offset within the target vertex where the edge originates from
   * @param targetOffset the offset within the target vertex where the edge originates from
   * @param graph the current points-to graph
   */
  def addNewTypeEdge(sourceVertex: DsOliVertexMemory, sourceOffset: Long, targetVertex: DsOliVertex, targetOffset: Long, graph: DsOliGraph): Unit = {
    val funSignature = classSignature + "addNewTypeEdge: "
    println(funSignature + "entered: ")
    targetVertex match {
      case tv: DsOliVertexMemory =>
        println(funSignature + " DsOliVertexMemory.")
        if (!graph.graph.edges.iterator.exists {
          edge =>
            edge.source.value == sourceVertex &&
              edge.target.value == targetVertex &&
              edge.sOffset == sourceOffset &&
              edge.tOffset == targetOffset
        }) {
          println(funSignature + " adding edge: " + sourceVertex.id + ":" + sourceOffset + "->" + targetVertex.id + ":" + targetOffset)
          // Here we always start from 0
          graph.add(sourceVertex ~> targetVertex toDsOliDiEdge (0, sourceOffset, 0, targetOffset))
        } else {
          println(funSignature + " edge already present: " + sourceVertex.id + ":" + sourceOffset + "->" + targetVertex.id + ":" + targetOffset)
        }

      case tv: DsOliVertexPredefined => // Nothing

      case _ =>
        throw new Exception("Vertex currently not supported: " + targetVertex)
    }
  }

  /**
   * Try to find an already existing type vertex for a given memory (!) vertex
   *
   * @param mergedTypeGraph the merged type graph
   * @param vertex a memory (!) vertex
   * @return Either a newly created type (!) vertex or vertex found in the current merged type graph
   */
  def findOrAddTypeVertex(mergedTypeGraph: DsOliGraph, vertex: DsOliVertexMemory): DsOliVertex = {
    val funSignature = classSignature + "findTypeVertex: "
    println(funSignature + " entered")

    // Search for existing vertex with the same type in the merged type graph
    val foundOpt = mergedTypeGraph.graph.nodes.iterator.find {
      n =>
        n.value match {
          case node: DsOliVertexMemory =>
            node.vType == vertex.vType
          case _ => false
        }
    }

    // Create new or return found vertex
    if (foundOpt.isEmpty) {
      DsOliLogger.debug(funSignature + " type not found: " + vertex.vType + " -> adding")
      val (typeID, isStack, offsetFromStart) = if (typeDB.getTypeIdentification(vertex.vType.vType).isDefined) {
        typeDB.getTypeIdentification(vertex.vType.vType).get
      } else {
        ("-", false, 0L)
      }
      val newVertex = createNewTypeVertex(vertex.vType, 0, vertex.vType.size, isStack, typeID, offsetFromStart)
      mergedTypeGraph.add(newVertex)
      newVertex
    } else {
      foundOpt.get.value
    }
  }

  /**
   * Try to find both the source and target vertex in the merged type graph.
   * If they are not found, they will be created.
   *
   * @param mergedTypeGraph the merged type graph
   * @param sourceVertex the source memory (!) vertex
   * @param targetVertex the target memory (!) vertex
   * @return Tuple with source and target type (!) vertices
   */
  def mergedTypeGraphFindOrAddVertices(mergedTypeGraph: DsOliGraph, sourceVertex: DsOliVertex, targetVertex: DsOliVertex): (Option[DsOliVertex], Option[DsOliVertex]) = {
    val funSignature = classSignature + "mergedTypeGraphFindOrAddVertices: "
    println(funSignature + " entered")

    val sourceVertexType = if (sourceVertex.isInstanceOf[DsOliVertexMemory])
      Some(findOrAddTypeVertex(mergedTypeGraph, sourceVertex.asInstanceOf[DsOliVertexMemory]))
    else None

    val targetVertexType = if (targetVertex.isInstanceOf[DsOliVertexMemory])
      Some(findOrAddTypeVertex(mergedTypeGraph, targetVertex.asInstanceOf[DsOliVertexMemory]))
    else None

    (sourceVertexType, targetVertexType)

  }

  /**
   * Process a memory write event
   *
   * @param event the memory write event
   * @param graph the current points-to graph
   * @param mergedTypeGraph the merged type graph
   */
  def processMemoryWrite(event: DsOliMWEvent, graph: DsOliGraph, mergedTypeGraph: DsOliGraph): Unit = {
    val funSignature = classSignature + "processMemoryWrite: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Only continue here, if:
    // a) malloc-like event
    // b) pointer write, where there already exists a vertex with the start address of the target
    if (event.isInstanceOf[DsOliNoAllocEvent]) {
      val targetAddr = event.rValue.content
      val targetTypeString = event.rValue.typeString
      val targetType =
        typeDB.getTypeObject(targetTypeString) match {
          case Some(t) => t
          case None => throw new Exception(funSignature + "Unknown type: " + targetTypeString)
        }

      val vertexEndAddress = targetAddr + targetType.size - 1

      val vertices = graph.graph.nodes

      val targetAddrInVertex = findVertexInRange(vertices, targetAddr, targetAddr).isEmpty
      val targetEndAddrInVertex = findVertexInRange(vertices, vertexEndAddress, vertexEndAddress).isDefined

      // Checks: is the start vertex outside of any given vertex AND is the end inside of any given vertex => overlap
      // Works at least for libusb-1.0.20 purposes
      if (targetAddrInVertex && targetEndAddrInVertex) {
        return
      }
    }

    // Fetch the context, if present
    val context =
      if (hasContext(event)) {
        event.lValueList.head
      } else {
        event.lValueList(0)
      }
    val contextAddress = context.address
    val sourceAddress = event.lValueList.last.address

    // Calculate the source vertex
    // Note: Use of type information
    val sourceType =
      typeDB.getTypeObject(context.typeString) match {
        case Some(t) => t
        case None => throw new Exception(funSignature + "Unknown type: " + context.typeString)
      }

    val sourceVertex = findOrAddVertex(contextAddress, sourceType, graph, sourceType.size)
    val sourceOffset = sourceAddress - sourceVertex.asInstanceOf[DsOliVertexMemory].bAddr

    DsOliLogger.debug(funSignature + "sourceVertex: " + sourceVertex +
      ", sourceAddress: " + sourceAddress + ", sourceOffset: " + sourceOffset)

    // Note: Use of type information
    // Type is already dereferenced in the trace, so nothing needed anymore
    val targetTypeString = event.rValue.typeString
    val targetType =
      typeDB.getTypeObject(targetTypeString) match {
        case Some(t) => t
        case None => throw new Exception(funSignature + "Unknown type: " + targetTypeString)
      }
    DsOliLogger.debug(funSignature + "target type: " + targetTypeString)

    // Calculate the target vertex
    var targetOffset: Long = 0
    val targetVertex = event.rValue.content match {
      // Should never happen here: UNDEF is only produced by free, VLS
      case 0 =>
        DsOliGraph.vertexNull
      case _ =>
        val targetSize = if (event.isInstanceOf[DsOliMallocEvent]) {
          DsOliLogger.debug(funSignature + "Got a malloc event: " + event.asInstanceOf[DsOliMallocEvent].argValue)
          DsOliLogger.debug("\t targetType: " + targetType + " targetType.size: " + targetType.size)
          event.asInstanceOf[DsOliMallocEvent].argValue.toLong
        } else {
          targetType.size
        }
        DsOliLogger.debug(funSignature + "findOrAddVertex: " + targetTypeString)
        val tVertex = findOrAddVertex(event.rValue.content, targetType, graph, targetSize)
        targetOffset = event.rValue.content - tVertex.asInstanceOf[DsOliVertexMemory].bAddr
        tVertex
    }

    // Update the merged type graph. Only add the vertex if it is not a void pointer -> avoid clutter
    if (sourceVertex.asInstanceOf[DsOliVertexMemory].vType.vType != "VOID *") {
      // Optional: avoid deep nestings created by Howard which are most likely noise 
      val maxNestingDepth = calculateMaximalNestingForType(sourceVertex.asInstanceOf[DsOliVertexMemory].vType, 0)
      if (maxNestingDepth < 5) {
        val (sourceVertexTypeOpt, targetVertexTypeOpt) = mergedTypeGraphFindOrAddVertices(mergedTypeGraph, sourceVertex, targetVertex)
        if (sourceVertexTypeOpt.isDefined && targetVertexTypeOpt.isDefined && targetVertexTypeOpt.get.isInstanceOf[DsOliVertexMemory])
          DsOliLogger.debug(funSignature + "Adding edge between: " + sourceVertexTypeOpt.get.asInstanceOf[DsOliVertexMemory] +
            " ::" + targetVertexTypeOpt.get.asInstanceOf[DsOliVertexMemory])
        addNewTypeEdge(sourceVertexTypeOpt.get.asInstanceOf[DsOliVertexMemory], sourceOffset,
          targetVertexTypeOpt.get.asInstanceOf[DsOliVertexMemory], targetOffset, mergedTypeGraph)
      }
    }

    DsOliLogger.debug(funSignature + "Adding ptg edge between: " + sourceVertex + " ::" + targetVertex)
    DsOliLogger.debug(funSignature + "targetVertex: " + targetVertex + ", targetOffset: " + targetOffset)

    // Update the points-to graph
    removeOldEdge(event, sourceVertex, event.lValueList.last.address, graph)
    addNewEdge(sourceVertex.asInstanceOf[DsOliVertexMemory], sourceOffset, targetVertex, targetOffset, graph)

  }

  /**
   * Calculate the maximal nesting level of a type
   * @param vType the type to check
   * @param maxNesting the nesting level counter
   * @return the deepest nesting level
   */
  def calculateMaximalNestingForType(vType: DsOliType, maxNesting: Int): Int = {
    var retMaxNesting = maxNesting
    if(vType.fields == null) return 0
    vType.fields.foreach {
      field =>
        if (field.fType == Compound) {
          val retNesting = calculateMaximalNestingForType(typeDB.getTypeObject(field.cType).get, maxNesting + 1)
          if (retNesting > retMaxNesting) {
            retMaxNesting = retNesting
          }
        }
    }
    retMaxNesting
  }

  /**
   * Calculate all edges pointing into the leaked vertices set
   * Operates on global leaked set.
   *
   * @param event the memory event
   * @param graphIMinusOne the points-to graph prior to the current time step (i - 1)
   * @param graphI the points-to graph at the current time step (i)
   * @return a list of edges pointing into the leaked vertices set
   */
  def calculateEdgesIntoLeaked(event: DsOliMemoryEvent, graphIMinusOne: DsOliGraph, graphI: DsOliGraph): ListBuffer[DsOliDiEdge[DsOliVertex]] = {
    val funSignature = classSignature + "calculateEdgesIntoLost: "
    DsOliLogger.debug(funSignature + "entered: ")
    var retEdges = new ListBuffer[DsOliDiEdge[DsOliVertex]]()
    val addressOpt = DsOliAddressUtils.getStartAddressFromFreeVLS(event)
    // Only do processing, if the event was a free or vls
    if (addressOpt.isDefined && graphI.getVertexForAddress(addressOpt.get).isDefined) {
      // Find the vertex which was freed
      val vertex = graphI.getMemoryVertex(graphI.getVertexForAddress(addressOpt.get).get)
      // Only process the nodes inside of the leaked set
      leaked.foreach {
        vertexTuple =>
          val (vertexId, curVertex) = vertexTuple
          DsOliLogger.debug(funSignature + "processing vertex of leaked set: " + curVertex)
          val innerVertex = graphI.graph.get(curVertex)
          // Test all the incoming edges to a vertex of the leaked set
          innerVertex.incoming.foreach {
            innerEdge =>
              val source = graphI.getMemoryVertex(graphI.getVertexForAddress(innerEdge.sAddr).get)
              val target = graphI.getMemoryVertex(graphI.getVertexForAddress(innerEdge.tAddr).get)
              if ( // Only interested in the freed vertex
              target == vertex
                // Do self reference test
                && source != vertex
                // Check, that the source is not inside the leaked set itself
                && !leaked.contains(source.id)) {
                DsOliLogger.debug(funSignature + "storing incoming edge for deletion: " + innerEdge.toEdgeIn)
                retEdges += innerEdge.toEdgeIn
              }
          }

      }
    }
    retEdges
  }

  /**
   * Calculate all edges pointing out of the leaked vertices set.
   * Operates on global leaked set.
   *
   * @param event the memory event
   * @param graphI the points-to graph at the current time step (i)
   * @return a list of edges pointing out of the leaked vertices set
   */
  def calculateEdgesOutgoingFromLeaked(graphI: DsOliGraph): ListBuffer[DsOliDiEdge[DsOliVertex]] = {
    val funSignature = classSignature + "calculateEdgesOutgoingFromLeaked: "
    DsOliLogger.debug(funSignature + "entered: ")
    val retEdges = new ListBuffer[DsOliDiEdge[DsOliVertex]]()
    leaked.foreach {
      vertexTuple =>
        val (_, vertex) = vertexTuple
        val innerVertex = graphI.graph.get(vertex)
        innerVertex.outgoing.foreach {
          innerEdge =>
            val edge = innerEdge.toEdgeIn
            // Make sure, that the outgoing edge is not pointing inside of the leaked set
            if (!leaked.contains(edge.to.id) && !edge.to.isInstanceOf[DsOliVertexNull] &&
              !edge.to.isInstanceOf[DsOliVertexUndef]) {
              DsOliLogger.debug(funSignature + "storing outgoing edge for deletion: " + edge)
              retEdges += edge
            }
        }
    }
    retEdges
  }

  // Keep track of the artificial events
  var numberOfArtificialEvents = 0

  /**
   * Create artificial events for the given edges which cut the edges by setting them to undef.
   * Additionally remove the edge from the graph directly.
   *
   * @param edges list of edges to remove
   * @param event the current event to store the artificial events to
   * @param graph the current points-to graph
   */
  def processEdges(edges: ListBuffer[DsOliDiEdge[DsOliVertex]], event: DsOliEvent, graph: DsOliGraph): Unit = {
    val funSignature = classSignature + "processEdges: "
    DsOliLogger.debug(funSignature + "entered: ")
    edges.foreach {
      outerEdge =>
        addNewEdge(outerEdge.source.asInstanceOf[DsOliVertexMemory], outerEdge.sOffset, DsOliGraph.vertexUndef, 0, graph)
        graph.graph -= outerEdge
        // Keep a record of the current graph in the set of artificial graphs
        graph.artificialGraphs += graph.deepCopy
        // Keep a record  of the artificial event, which was just executed
        val lValueList = new ListBuffer[DsOliLValue]()
        lValueList += new DsOliLValue(outerEdge.sAddr + outerEdge.sOffset, "", "")
        val rValue = new DsOliRValue(-1, "", "")
        val artEvent = new DsOliArtificialUndefEvent(numberOfArtificialEvents, event.sourceLocation,
          lValueList, rValue, outerEdge.sAddr, outerEdge.sOffset)
        DsOliLogger.debug(funSignature + "created new artificial undef event: " + artEvent)
        event.artificialEvents += artEvent
        numberOfArtificialEvents += 1
    }
  }

  /**
   * Remove a vertex and record an artificial event for that
   *
   * @param vertex the memory vertex to remove
   * @param event the event which will recored the artificial event
   * @param graph the current points-to graph
   */
  def removeVertex(vertex: DsOliVertexMemory, event: DsOliEvent, graph: DsOliGraph): Unit = {
    removeNodeAndAssociatedEdges(vertex, graph)
    val funSignature = classSignature + "removeVertex: "
    DsOliLogger.debug(funSignature + "entered: ")
    // Keep a record of the current graph in the set of artificial graphs
    graph.artificialGraphs += graph.deepCopy
    // Keep a record  of the artificial event, which was just executed
    val artEvent = new DsOliArtificialFreeEvent(numberOfArtificialEvents, event.sourceLocation, "", vertex.bAddr)
    DsOliLogger.debug(funSignature + "created new artificial free event: " + artEvent)
    event.artificialEvents += artEvent
    numberOfArtificialEvents += 1

  }

  /**
   * Linearize the in/out going edges to leaked vertex set
   *
   * @param event the event which triggered the leak (can also be a free)
   * @param graphI the current points-to graph
   * @param incomingEdges all edges pointing into the leaked set
   * @param outgoingEdges all edges pointing out of the leaked set
   */
  def linearizeAsEvents(event: DsOliEvent, graphI: DsOliGraph, incomingEdges: ListBuffer[DsOliDiEdge[DsOliVertex]],
    outgoingEdges: ListBuffer[DsOliDiEdge[DsOliVertex]]): Unit = {
    val funSignature = classSignature + "linearizeAsEvents: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Reset the number of artificial events
    numberOfArtificialEvents = 0

    // In case of a pointer write, store the already modified graph + event in the 
    // artificial sub events/graphs
    if (event.isInstanceOf[DsOliNoAllocEvent]) {
      DsOliLogger.debug(funSignature + "event is NoAllocEvent: inserting as first item of artificials: " + event)
      graphI.artificialGraphs += graphI.deepCopy
      event.artificialEvents += event
      numberOfArtificialEvents += 1
    }
    // Cut all incoming edges
    // The pointers are still there, but are not pointing to something meaningful.
    processEdges(incomingEdges, event, graphI)

    // Cut all outgoing edges
    // literally not present after destruction of
    // the leaked set
    processEdges(outgoingEdges, event, graphI)

    // Remove all vertices
    leaked.foreach {
      vertexTuple =>
        val (vertexId, vertex) = vertexTuple
        removeVertex(vertex, event, graphI)
    }
  }

  /**
   * Remove a vertex and its associated edges (in/out)
   *
   * @param vertex the vertex to remove
   * @param graph the current points-to graph
   */
  def removeNodeAndAssociatedEdges(vertex: DsOliVertex, graph: DsOliGraph): Unit = {
    val funSignature = classSignature + "removeNodeAndAssociatedEdges: "
    DsOliLogger.debug(funSignature + "entered: ")
    val innerVertex = graph.graph.get(vertex)

    val deleteEdges = Set[DsOliDiEdge[DsOliVertex]]()

    DsOliLogger.debug(funSignature + "processing incoming edges: ")

    // Process incoming edges
    innerVertex.incoming.foreach {
      innerEdge =>

        if (innerEdge != null) {
          val outerEdge = innerEdge.toEdgeIn
          if (outerEdge != null) {
            DsOliLogger.debug(funSignature + "processing incoming edge: " + outerEdge)
            addNewEdge(outerEdge.source.asInstanceOf[DsOliVertexMemory], outerEdge.sOffset, DsOliGraph.vertexUndef, 0, graph)
            deleteEdges.add(outerEdge)
          }
        }
    }

    DsOliLogger.debug(funSignature + "done collecting edges for deletion: " + deleteEdges.size)
    deleteEdges.foreach {
      edge =>
        DsOliLogger.debug(funSignature + "deleting edge: " + edge)
        graph.graph -= edge
    }
    DsOliLogger.debug(funSignature + "deletion done.")

    // Reset edges for deletion
    deleteEdges.clear

    // Process outgoing edges
    DsOliLogger.debug(funSignature + "processing outgoing edges: ")
    innerVertex.outgoing.foreach {
      innerEdge =>
        condOpt(innerEdge.to.value) {
          // Only process outgoing edges which are not pointing to NULL/Undef anyway
          case to: DsOliVertexMemory =>
            val outerEdge = innerEdge.toEdgeIn
            DsOliLogger.debug(funSignature + "processing outgoing edge: " + outerEdge)
            deleteEdges.add(outerEdge)
        }

    }

    DsOliLogger.debug(funSignature + "done collecting edges for deletion: " + deleteEdges.size)

    deleteEdges.foreach {
      edge =>
        DsOliLogger.debug(funSignature + "deleting edge: " + edge)
        graph.graph -= edge
    }
    DsOliLogger.debug(funSignature + "deletion done.")

    DsOliLogger.debug(funSignature + "removing vertex")
    // Remove vertex
    graph.graph -= vertex

  }

  /**
   * Handle a variable entering scope event
   *
   * @param event the variable enter scope event
   * @param graph the current points-to graph
   */
  def processVES(event: DsOliVESEvent, graph: DsOliGraph): Unit = {
    val funSignature = classSignature + "processVES: "
    val varType =
      typeDB.getTypeObject(event.varType) match {
        case Some(t) => t
        case None => throw new Exception(classSignature + "Unknown type: " + event.varType)
      }
    val sourceVertex = findOrAddVertex(event.address, varType, graph, varType.size, isStack = true)
  }

  /**
   * Handle a variable leaving scope event
   *
   * @param event the variable leaving scope event
   * @param graph the current points-to graph
   */
  def processVLS(event: DsOliVLSEvent, graph: DsOliGraph): Unit = {
    val funSignature = classSignature + "processVLS: "
    DsOliLogger.debug(funSignature + "entered. event: " + event)
    val vertexOpt = graph.getVertexForAddress(event.address)
    if (vertexOpt.isDefined) {
      removeNodeAndAssociatedEdges(vertexOpt.get, graph)
    } else {
      DsOliLogger.error(funSignature + "no vertex found for address: " + event.address.toHexString)

    }
  }

  // Bookkeeping for leak detection

  // Keep track of reachable vertices
  var reachable = new HashMap[Long, DsOliVertexMemory]()
  // Keep track of leaked vertices
  var leaked = new HashMap[Long, DsOliVertexMemory]()
  // Keep track of what to process
  var processQueue = new Queue[DsOliVertexMemory]()
  // Keep track of what was already processed
  var processed = new HashMap[Long, DsOliVertexMemory]()
  // Keep track with which vertex the leak detection started
  var startId: Long = 0

  /**
   * Generic function to calculate the vertices which
   * are either directly reachable by one hop following the
   * outgoing edges or the incoming edges
   *
   * @param vertex the vertex for the in/out going vertices
   * @param graph the current points-to graph
   * @param testAddressProducer function reference for producing the address to test (either source or target)
   * @param returnAddressProducer function reference for producing the address to test against (either source or target)
   */
  def getEitherVertices(vertex: DsOliVertexMemory, graph: DsOliGraph,
    testAddressProducer: (DsOliDiEdge[DsOliVertex]) => Long,
    returnAddressProducer: (DsOliDiEdge[DsOliVertex]) => Long): ListBuffer[DsOliVertexMemory] = {
    val funSignature = classSignature + "getEitherVertices: "
    DsOliLogger.debug(funSignature + "entered: ")
    var retVertices = new ListBuffer[DsOliVertexMemory]

    val iter = graph.graph.edges.iterator
    while (iter.hasNext) {
      val edgeOuter = iter.next.toOuter
      DsOliLogger.debug(funSignature + "processing edgeOuter: " + edgeOuter)
      val test = getVertexOpt(edgeOuter, graph, testAddressProducer)
      DsOliLogger.debug(funSignature + "testing against vertex: " + vertex)
      DsOliLogger.debug(funSignature + "testing.isDefined: " + test.isDefined)
      if (test.isDefined && test.get.asInstanceOf[DsOliVertex] == vertex.asInstanceOf[DsOliVertex]) {
        val vertexOpt = getVertexOpt(edgeOuter, graph, returnAddressProducer)
        DsOliLogger.debug(funSignature + "vertexOpt.isDefined: " + vertexOpt.isDefined)
        // Processed vertices are not of interest, except the first one
        if (vertexOpt.isDefined && (startId == vertexOpt.get.id || !processed.contains(vertexOpt.get.id))) {
          DsOliLogger.debug(funSignature + "Found vertex to add: " + vertexOpt.get)
          retVertices += vertexOpt.get
        } else if (vertexOpt.isDefined) {
          DsOliLogger.debug(funSignature + "Vertex not needed: " + vertexOpt.get)
        } else {
          DsOliLogger.debug(funSignature + "Vertex not defined")
        }
      } else if (test.isEmpty) {
        DsOliLogger.debug(funSignature + "test.isEmpty! edgeOuter = " + edgeOuter)
      } else {
        DsOliLogger.debug(funSignature + "test.isDefined: test.get: " + test.get)
        DsOliLogger.debug(funSignature + "test.isDefined: test == vertex: " + (test.get.asInstanceOf[DsOliVertex] == vertex.asInstanceOf[DsOliVertex]))
      }
    }

    DsOliLogger.debug(funSignature + "following vertices were found: ")
    retVertices.foreach {
      vert =>
        DsOliLogger.debug(funSignature + "\tfound vertex: " + vert)
    }
    retVertices
  }

  /**
   * Fetch the vertex from an edge with a given address producer
   *
   * @param edge the edge which gets passed to the address producer
   * @param graph the current points-to graph
   * @param addressProducer function reference to producer either the source or target address of the edge
   * @return Option of a memory vertex
   */
  def getVertexOpt(edge: pointstograph.DsOliDiEdge[DsOliVertex], graph: DsOliGraph, addressProducer: (DsOliDiEdge[DsOliVertex]) => Long): Option[DsOliVertexMemory] = {
    val funSignature = classSignature + "getVertexOpt: "
    DsOliLogger.debug(funSignature + "entered: ")
    val address = addressProducer(edge)
    if (address != 0) {
      Some(graph.getMemoryVertex(graph.getVertexForAddress(address).get))
    } else {
      None
    }
  }

  /**
   * Address producer, that fetches the source address from an edge
   *
   * @param edge the edge
   * @return the source address of the edge
   */
  def getSourceAddress(edge: DsOliDiEdge[DsOliVertex]): Long = {
    val funSignature = classSignature + "getSourceAddress: "
    DsOliLogger.debug(funSignature + "entered: ")
    edge.sAddr
  }

  /**
   * Address producer, that fetches the target address from an edge
   *
   * @param edge the edge
   * @return the target address of the edge
   */
  def getTargetAddress(edge: DsOliDiEdge[DsOliVertex]): Long = {
    val funSignature = classSignature + "getTargetAddress: "
    DsOliLogger.debug(funSignature + "entered: ")
    edge.tAddr
  }

  /**
   * Fetch all vertices which are directly reachable from this vertex,
   * i.e., by only following one pointer/edge hop
   *
   * @param vertex the vertex from which the outgoing edges originate
   * @param graph the current points-to graph
   * @return a list of the directly reachable vertices
   */
  def getOutgoingVertices(vertex: DsOliVertexMemory, graph: DsOliGraph): ListBuffer[DsOliVertexMemory] = {
    val funSignature = classSignature + "getOutgoingVertices: "
    DsOliLogger.debug(funSignature + "entered: " + vertex)
    // Anonymous functions to produce the source vertex for testing and target vertex for returning
    getEitherVertices(vertex, graph, getSourceAddress, getTargetAddress)
  }

  /**
   * Fetch all vertices which are directly pointing to this vertex,
   * i.e., by only following one pointer/edge hop
   *
   * @param vertex the vertex from which the outgoing edges originate
   * @param graph the current points-to graph
   * @return a list of the vertices directly pointing to the vertex
   */
  def getIncomingVertices(vertex: DsOliVertexMemory, graph: DsOliGraph): ListBuffer[DsOliVertexMemory] = {
    val funSignature = classSignature + "getIncomingVertices: "
    DsOliLogger.debug(funSignature + "entered: " + vertex)
    // Anonymous functions to produce the target vertex for testing and source vertex for returning
    getEitherVertices(vertex, graph, getTargetAddress, getSourceAddress)
  }

  /**
   * Test if a given vertex is reachable, i.e., there exists a
   * pointer chain that ends in static memory.
   *
   * @param padding string for indicating the current recursion level for debugging
   * @param vertex the vertex under test
   * @param Pin bread crumb to avoid cycles
   * @param graph the current points-to graph
   * @return Boolean
   */
  def reachable(padding: String, vertex: DsOliVertexMemory, Pin: HashMap[Long, DsOliVertexMemory], graph: DsOliGraph): Boolean = {
    val funSignature = padding + classSignature + "reachable: "
    DsOliLogger.debug(funSignature + "entered: ")
    Pin.put(vertex.id, vertex)
    if (reachable.contains(vertex.id) || DsOliAddressUtils.isStatic(vertex)) {
      DsOliLogger.debug(funSignature + "vertex is reachable: " + vertex)
      reachable.put(vertex.id, vertex)
      return true
    }

    // Fetch all vertices pointing to this vertex
    val incomingVertices = getIncomingVertices(vertex, graph)

    // Cut off immediately, if one of the incoming pointers is static and thus makes this vertex reachable
    if (incomingVertices.exists { vertex =>
      DsOliLogger.debug(funSignature + " testing shortcut static on vertex: " + vertex)
      !processed.contains(vertex.id) && DsOliAddressUtils.isStatic(vertex)
    }) {
      DsOliLogger.debug(funSignature + "vertex is reachable")
      return true
    }

    // Check all incoming vertices recursively
    incomingVertices.foreach {
      vin =>
        DsOliLogger.debug(funSignature + "processing vertex = " + vin)
        if (!leaked.contains(vin.id) && !Pin.contains(vin.id)) {
          DsOliLogger.debug(funSignature + "not yet processed")
          if (reachable(padding + "    ", vin, Pin, graph)) {
            DsOliLogger.debug(funSignature + "vertex is reachable: " + vin)
            return true
          }
        }
    }
    DsOliLogger.debug(funSignature + "vertex is leaked: " + vertex)

    return false
  }

  /**
   * Check for leaks
   *
   * @param graph the current points-to graph
   */
  def checkLeak(graph: DsOliGraph): Unit = {
    val funSignature = classSignature + "checkLeak: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Get the element, which is already marked as processed
    // as the processed list gets cleared during each iteration
    // and thus needs to be re initialized again
    val processedListFilled = processed.size > 0
    var (initVertexId, initVertex): (Long, DsOliVertexMemory) = (0, null)
    if (processedListFilled) {
      initVertexId = processed.toList.head._1
      initVertex = processed.toList.head._2
    }

    // Fetch vertices to check from the process queue
    while (!processQueue.isEmpty) {
      val vertex = processQueue.dequeue

      // Always clear the processed list
      processed.clear

      // In case it was filled, refill it
      if (processedListFilled) {
        processed.put(initVertexId, initVertex)
      }

      DsOliLogger.debug(funSignature + "processing vertex " + vertex)
      if (!processed.contains(vertex.id)) {
        DsOliLogger.debug(funSignature + "not processed yet.")
        processed.put(vertex.id, vertex)
        if (leaked.contains(vertex.id) || !reachable("", vertex, new HashMap[Long, DsOliVertexMemory], graph)) {
          DsOliLogger.debug(funSignature + "vertex is leaked: " + vertex)
          leaked.put(vertex.id, vertex)
          val outgoingVertices = getOutgoingVertices(vertex, graph)
          outgoingVertices.foreach {
            outgoingVertex =>
              if (!processed.contains(outgoingVertex.id) && !leaked.contains(outgoingVertex.id)) {
                processQueue.enqueue(outgoingVertex)
                DsOliLogger.debug(funSignature + "got following outgoing vertex: " + outgoingVertex)
              } else {

                DsOliLogger.debug(funSignature + "skipping following outgoing vertex because already processed or leaked: " + outgoingVertex)
              }
          }
        }
      }
    }
  }

  /**
   * Fetch the memory vertex for a free, variable leaving scope event
   *
   * @param address the address to free
   * @param ptgI the current points-to graph
   * @return Option the memory vertex
   */
  def getFreeVLSMemoryVertex(address: Long, ptgI: DsOliGraph): Option[DsOliVertexMemory] = {
    val funSignature = classSignature + "getFreeVLSMemoryVertex: "
    DsOliLogger.debug(funSignature + "entered. address = " + address.toHexString)
    val vOpt = ptgI.getVertexForAddress(address)
    if (vOpt.isDefined) {
      DsOliLogger.debug(funSignature + "found vOpt = " + vOpt.get)
      Some(ptgI.getMemoryVertex(vOpt.get))
    } else {
      DsOliLogger.debug(funSignature + "not found vOpt")
      None
    }
  }

  /**
   * Initializes the leak check. In case of a free/VLS the
   * vertex is always placed in the leaked set, to naturally
   * reuse the algorithm to perform the actual free/VLS.
   * This method is applied to all memory events as also
   * pointer writes can cause leaks.
   *
   * @param event the memory event
   * @param ptgIMinusOne the points-to graph of the previous time step (i-1)
   * @param ptgI the points-to graph of the current time step (i)
   */
  def initCheckLeak(event: DsOliMemoryEvent, ptgIMinusOne: DsOliGraph, ptgI: DsOliGraph) {
    val funSignature = classSignature + "initCheckLeak: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Reset everything
    reachable.clear
    leaked.clear
    processed.clear
    processQueue.clear

    var vertex: DsOliVertexMemory = null

    val addressOpt = DsOliAddressUtils.getStartAddressFromFreeVLS(event)
    // Free & VLS
    if (addressOpt.isDefined && addressOpt.get != 0) {
      DsOliLogger.debug(funSignature + "addressOpt = " + addressOpt.get.toHexString)
      val vOpt = getFreeVLSMemoryVertex(addressOpt.get, ptgI)
      if (vOpt.isDefined) {
        vertex = vOpt.get
        val outgoingVertices = getOutgoingVertices(vertex, ptgI)
        outgoingVertices.foreach(processQueue.enqueue(_))

        startId = vertex.id
        processed.put(vertex.id, vertex)

        // In case of free | vls this vertex is always "leaked" in order to remove it
        leaked.put(vertex.id, vertex)

      }
      // Pointer write
    } else if (event.isInstanceOf[DsOliMWEvent]) {

      // Need PTG(i-1) here to find out the old address
      val sourceAddress = event.asInstanceOf[DsOliMWEvent].lValueList.last.address
      val targetAddressOpt = ptgIMinusOne.getCurrentTargetAddr(sourceAddress) //event.rValue.content

      if (targetAddressOpt.isDefined && targetAddressOpt.get != 0) {
        DsOliLogger.debug(funSignature + "targetAddressOpt: = " + targetAddressOpt.get.toHexString)
        DsOliLogger.debug(funSignature + "sourceAddress: = " + sourceAddress.toHexString)
        // Try to get the found vertex from the current PTG(i)!
        vertex = ptgI.getMemoryVertex(ptgI.getVertexForAddress(targetAddressOpt.get).get)
        processQueue.enqueue(vertex)

        // Here the recording of the source vertex is easy, so do the speedup (compare to free/vls remark above)
        val sourceVertex = ptgI.getMemoryVertex(ptgI.getVertexForAddress(sourceAddress).get)
        processed.put(sourceVertex.id, sourceVertex)
        startId = sourceVertex.id
      }
    }
    DsOliLogger.debug(funSignature + "initCheckLeak: processQueue = " + processQueue)
    DsOliLogger.debug(funSignature + "initCheckLeak: processed = " + processed)
  }

  /**
   * Calculate the changes for the current points-to graph given a memory event and
   * additionally calculate the merged type graph used by the Howard refinement step
   *
   * @param event the memory event
   * @param graph the current points-to graph
   * @param mergedTypeGraph the merged type graph
   */
  def processMemoryEvent(event: DsOliMemoryEvent, graph: DsOliGraph, mergedTypeGraph: DsOliGraph): Unit = {
    val funSignature = classSignature + "processMemoryEvent: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Copy the graph immediately: No modifications yet -> last graph
    val ptgIMinusOne = graph.deepCopy

    condOpt(event) {
      case e: DsOliMWEvent =>
        processMemoryWrite(e, graph, mergedTypeGraph)
      case e: DsOliVESEvent =>
        processVES(e, graph)
      case e: DsOliVLSEvent =>
      //processVLS(e, graph)
    }

    // The check/cleanup for/of memory leaks needs
    // to be performed on all of those events
    if (event.isInstanceOf[DsOliMWEvent] ||
      event.isInstanceOf[DsOliFreeEvent] ||
      event.isInstanceOf[DsOliVLSEvent]) {
      initCheckLeak(event, ptgIMinusOne, graph)

      // Do the leak detection on the current graph
      checkLeak(graph)

      DsOliLogger.debug(funSignature + "testing for free/vls: " + event)
      // In case of a free/vls, remove the vertex as the last element
      val addressOpt = DsOliAddressUtils.getStartAddressFromFreeVLS(event)
      // Free & VLS
      if (addressOpt.isDefined && addressOpt.get != 0) {
        DsOliLogger.debug(funSignature + "event is free/vls: trying to insert into artificials: " + event)
        val vOpt = getFreeVLSMemoryVertex(addressOpt.get, graph)
        if (vOpt.isDefined) {
          DsOliLogger.debug(funSignature + "event is free/vls: testing for artificials: " + event)
          val vertex = vOpt.get

          // Always put the freed vertex itself into our set of lost vertices
          // leaked.clear
          leaked.put(vertex.id, vertex)

        }
      } else {
        DsOliLogger.debug(funSignature + "not defined: addressOpt.isDefined: " + addressOpt.isDefined)
      }
      if (leaked.size == 0) {
        DsOliLogger.debug(funSignature + "No leak detected.")
      } else {

        DsOliLogger.debug(funSignature + "Leak detected:")
        leaked.foreach(l => DsOliLogger.debug(funSignature + "Leaked: " + l))
        //Linearize as events...
        linearizeAsEvents(event, graph, calculateEdgesIntoLeaked(event, ptgIMinusOne, graph),
          calculateEdgesOutgoingFromLeaked(graph))

      }
    }

  }

  /**
   * Cleanup of unused vertices
   *
   * @param graph the current points-to graph
   */
  def remAbandondedVertices(graph: DsOliGraph): Unit = {

    val funSignature = classSignature + "remAbandondedVertices: "
    val delVertices = new ListBuffer[DsOliVertex]()
    graph.graph.nodes.iterator.foreach {
      node =>
        if (node.incoming.size == 0) {
          if (node.value.isInstanceOf[DsOliVertexMemory]) {
            val vertex = node.value.asInstanceOf[DsOliVertexMemory]
            if (!DsOliAddressUtils.isStatic(vertex)) {
              println(funSignature + "node.incoming.size: " + node.incoming.size + " vertex: " + vertex)
              delVertices.append(node.value)
            }
          }
        }
    }
    delVertices.foreach {
      del =>
        graph.graph.remove(del)
    }
  }

  /**
   * Calculate a points-to graph (PTG) for each time step
   * of the event trace. Additionally create a merged
   * type graph required for the refinement step
   *
   * @return returns a tuple (DsOliPointsToGraphs containing all PTGs, merged type graph)
   */
  def createPTGs(): (DsOliPointsToGraphs, DsOliGraph) = {
    val funSignature = classSignature + "createPTGs: "
    DsOliLogger.debug(funSignature + "entered: ")
    var ptg = new DsOliPointsToGraphs()

    // Holds a merged view of all types seen by the program
    var mergedTypeGraphs = new DsOliPointsToGraphs()
    var mergedTypeGraph = new DsOliGraph(mergedTypeGraphs)

    // Set the PTG at index 0
    ptg.append(createInitialPTG(ptg))

    // Only fetch the memory events
    var timeStep = 1
    events.events.foreach {
      event =>
        DsOliLogger.debug(funSignature + "#Event " + event.id
          + "# Processing ptg event " + event.id + " at time step: " + timeStep)
        // Fetch a copy of the points-to graph from the previous time step
        val curPtg = ptg.getCopyFromStep(timeStep - 1)

        // Only process memory events
        condOpt(event) {
          case e: DsOliMemoryEvent =>
            DsOliLogger.debug(funSignature + "Memory event")
            processMemoryEvent(e, curPtg, mergedTypeGraph)
        }

        DsOliLogger.debug(funSignature + "#Done Event " + event.id
          + "# Done ptg event " + event.id + " at time step: " + timeStep)

        // Save the possibly modified current points-to graph
        ptg.append(curPtg)

        timeStep += 1
        print("Ptg: " + timeStep + "/" + events.events.size + "\r")
    }

    DsOliTestMethods.printPTG(mergedTypeGraph, typeDB, None, null, 0, 0, null, "_merged_type_graph_", false, false)

    return (ptg, mergedTypeGraph)
  }
}