
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
 * DsOliEventParser.scala created on Sep 22, 2014
 *
 * Description: Parse the event trace XML produced
 * by the external instrumentation framework
 */
package event

import java.io.File
import java.io.FileInputStream
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import xmltools.XMLXSDLoader
import scala.xml.Node
import scala.collection.mutable.ListBuffer
import extlogger._
import pointstograph.ITypeDB
import event.DsOliEventParser._
import xmltools.DsOliXMLHelper

/**
 * @author DSI
 *
 */
class DsOliEventParser(xmlFilePath: String, xsdFilePath: String, typeDB: ITypeDB) extends IDsOliEventParser {

  val classSignature = "DsOliEventParser::"

  val HexBase = 16

  /**
   * Fetch the source location of an event
   *
   * @param sourceLocationOpt Option of event node
   * @returns instance of DsOliSourceLocation containing the source location information
   */
  def getSourceLocation(sourceLocationOpt: Option[Node]): DsOliSourceLocation = {
    return if (sourceLocationOpt.isDefined) {
      val sourceLocation = sourceLocationOpt.get
      val sourceLine: String = DsOliXMLHelper.valueOrZero(DsOliXMLHelper.getTagValue(sourceLocation,
        DsOliEventConfig.SourceLine))
      val sourceLineColumn: String = DsOliXMLHelper.valueOrZero(DsOliXMLHelper.getTagValue(sourceLocation,
        DsOliEventConfig.SourceLineColumn))

      new DsOliSourceLocation(
        DsOliXMLHelper.getTagValue(sourceLocation, DsOliEventConfig.SourceFile),
        sourceLine.toInt,
        sourceLineColumn.toInt)
    } else {
      new DsOliSourceLocation("", 0, 0)
    }
  }

  /**
   * Parse the operation transition event
   *
   * @param id the event id
   * @param event the event node
   * @return an instance of the event object
   */
  def oTEvent(id: Int, event: Node): DsOliEvent = {

    // Get the operation-transition node
    val operationTransition = event.child.head

    // Get the entry/exit node
    val eventType = operationTransition.child.head

    return eventType.label match {
      case DsOliEventConfig.Entry => new DsOliOTEntryEvent(
        id,
        getSourceLocation(Some(event)),
        DsOliXMLHelper.getTagValue(eventType, DsOliEventConfig.Id).toInt,
        DsOliXMLHelper.getTagValue(eventType, DsOliEventConfig.Name),
        DsOliXMLHelper.getTagValue(eventType, DsOliEventConfig.Argument),
        DsOliXMLHelper.getTagValue(eventType, DsOliEventConfig.Comment))
      case DsOliEventConfig.Exit => new DsOliOTExitEvent(id, getSourceLocation(Some(event)))
      case _ => throw new Exception("DsOliEventParser::oTEvent: Invalid type")
    }

  }

  /**
   * Parse the block transition event
   *
   * @param id the event id
   * @param event the event node
   * @return an instance of the event object
   */
  def bTEvent(id: Int, event: Node): DsOliEvent = {
    // Get the block-transition node
    val blockTransition = event.child.head

    // Get the sourceLocation node
    val sourceLocationOpt = DsOliXMLHelper.getTagNode(blockTransition, DsOliEventConfig.SourceLocation)

    return DsOliXMLHelper.getTagValue(blockTransition, DsOliEventConfig.Kind) match {
      case DsOliEventConfig.Entry => new DsOliBTEntryEvent(
        id,
        getSourceLocation(sourceLocationOpt))
      case DsOliEventConfig.Exit => new DsOliBTExitEvent(
        id,
        getSourceLocation(sourceLocationOpt))
      case _ => throw new Exception("Invalid type")
    }

  }

  /**
   * Checks, if the left value has a context, i.e., direct pointer assignment or not
   *
   * @param lValueList a list representing the lvalues
   * @return Boolean
   */
  def hasContext(lValueList: ListBuffer[DsOliLValue]): Boolean = {
    // exactly one member means, that we are directly assigning to a pointer
    // => more members means context
    return lValueList.length > 1
  }

  /**
   * Get the source address and the source offset
   *
   * @param lValueList a list representing the lvalues
   * @return tuple (source address, source offset)
   */
  def calculateAddressAndOffset(lValueList: ListBuffer[DsOliLValue]): (AddressOffsetTuple) = {
    var sourceOffset: Long = 0
    // Check for pointer context. E.g.: outer.inner.ptr
    val context =
      if (hasContext(lValueList)) {
        sourceOffset = (lValueList(lValueList.length - 1).address) - lValueList.head.address
        lValueList.head
      } else {
        lValueList(0)
      }
    val sourceAddress = context.address

    // Note: Use of type information
    val sourceType =
      typeDB.getTypeObject(context.typeString) match {
        case Some(t) => t
        case None => throw new Exception("Unknown type: " + context.typeString)
      }

    return (sourceAddress, sourceOffset)
  }

  /**
   * Take care of special sequences for representing NULL in the XML
   * and remove 0x in front of the string
   * Example: "(nil)"
   *
   * @param address the address to convert
   * @param returns the converted address
   */
  def convertAddressString(address: String): String = {
    return (if (address == DsOliEventConfig.Nil) "0" else address).replace("0x", "")
  }

  /**
   * Create the lvalue representation of the XML node
   *
   * @param node the XML node
   * @returns instance of DsOliLValue
   */
  def createLvalue(node: Node): DsOliLValue = {
    DsOliLogger.debug("DsOliEventParser::createLvalue: entered. ")
    val address = DsOliXMLHelper.getTagValue(node, DsOliEventConfig.Address)
    val convertedAddress = convertAddressString(address)
    val parsedAddress = DsOliXMLHelper.parseLong(convertedAddress, HexBase)
    DsOliLogger.debug("DsOliEventParser::createLvalue: address: " + address + ", convertedAddress: "
      + convertedAddress + ", parsedAddress: " + parsedAddress)
    new DsOliLValue(
      parsedAddress,
      DsOliXMLHelper.getTagValue(node, DsOliEventConfig.Type),
      DsOliXMLHelper.getTagValue(node, DsOliEventConfig.CodeFragment))
  }

  /**
   * Parse the memory write event
   *
   * @param id the event id
   * @param event the event node
   * @return an instance of the event object
   */
  def mWEvent(id: Int, event: Node): DsOliEvent = {
    // Get the memory-write node
    val memoryWrite = event.child.head;

    // Get the sourceLocation node and object
    val sourceLocation = DsOliXMLHelper.getTagNode(memoryWrite, DsOliEventConfig.SourceLocation)
    val sourceLocationObj = getSourceLocation(sourceLocation)

    // Get the lvalue nodes and create the objects
    val contextOpt = DsOliXMLHelper.getTagNode(memoryWrite, DsOliEventConfig.Context)
    val lvalOpt = DsOliXMLHelper.getTagNode(memoryWrite, DsOliEventConfig.Lvalue)
    var lValueObjects = new ListBuffer[DsOliLValue]()
    if (contextOpt.isDefined) {
      lValueObjects.append(createLvalue(contextOpt.get))
    }
    if (lvalOpt.isDefined) {
      lValueObjects.append(createLvalue(lvalOpt.get))
    }

    val (sourceAddress, sourceOffset) = calculateAddressAndOffset(lValueObjects)

    // Get the rvalue node and object
    val contentOpt = DsOliXMLHelper.getTagNode(memoryWrite, DsOliEventConfig.Content)
    val rValueObj = if (contentOpt.isDefined) {
      val content = contentOpt.get
      new DsOliRValue(
        DsOliXMLHelper.parseLong(convertAddressString(DsOliXMLHelper.getTagValue(content, DsOliEventConfig.Content)), HexBase),
        DsOliXMLHelper.getTagValue(content, DsOliEventConfig.RhsCodeFragment),
        DsOliXMLHelper.getTagValue(content, DsOliEventConfig.LvalDerefType))
    } else {
      throw new Exception("DsOliEventParser::mWEvent: content was not found in memory write event: " + memoryWrite)
    }

    // Get the memory allocation node and object
    val memoryAllocOpt = DsOliXMLHelper.getTagNode(memoryWrite, DsOliEventConfig.MemoryAllocation)
    val memoryEvent: DsOliMWEvent =
      if (memoryAllocOpt.isDefined && memoryAllocOpt.get.child.length != 0) {
        val memoryAlloc = memoryAllocOpt.get
        memoryAlloc.child.head.label match {
          case DsOliEventConfig.Malloc =>
            val mallocOpt = DsOliXMLHelper.getTagNode(memoryAlloc, DsOliEventConfig.Malloc)
            new DsOliMallocEvent(
              id,
              sourceLocationObj,
              lValueObjects,
              rValueObj,
              sourceAddress,
              sourceOffset,
              DsOliXMLHelper.getTagValue(mallocOpt.get, DsOliEventConfig.ArgCodeFragment),
              DsOliXMLHelper.getTagValue(mallocOpt.get, DsOliEventConfig.ArgValue))
          case DsOliEventConfig.Calloc =>
            val callocOpt = DsOliXMLHelper.getTagNode(memoryAlloc, DsOliEventConfig.Calloc)
            new DsOliCallocEvent(
              id,
              sourceLocationObj,
              lValueObjects,
              rValueObj,
              sourceAddress,
              sourceOffset,
              DsOliXMLHelper.getTagValue(callocOpt.get, DsOliEventConfig.ArgCodeFragment),
              DsOliXMLHelper.getTagValue(callocOpt.get, DsOliEventConfig.ArgChunkSize).toInt,
              DsOliXMLHelper.getTagValue(callocOpt.get, DsOliEventConfig.ArgTimes).toInt)
          case _ => throw new Exception("Invalid type")
        }
      } else {
        new DsOliNoAllocEvent(
          id,
          sourceLocationObj,
          lValueObjects,
          rValueObj,
          sourceAddress,
          sourceOffset)
      }

    return memoryEvent
  }

  /**
   * Parse the variable enter scope event
   *
   * @param id the event id
   * @param event the event node
   * @return an instance of the event object
   */
  def vESEvent(id: Int, event: Node): DsOliEvent = {
    // Get the variable-enter-scope node
    val varEnterScopeOpt = DsOliXMLHelper.getTagNode(event, DsOliEventConfig.VariableEnterScope)
    return if (varEnterScopeOpt.isDefined) {
      val varEnterScope = varEnterScopeOpt.get
      DsOliLogger.debug("DsOliEventParser::vESEvent: varEnterScope: " + varEnterScope)
      val address = convertAddressString(DsOliXMLHelper.getTagValue(varEnterScope, DsOliEventConfig.Address))
      DsOliLogger.debug("DsOliEventParser::vESEvent: address: " + address)
      new DsOliVESEvent(
        id,
        getSourceLocation(Some(event)),
        DsOliXMLHelper.getTagValue(varEnterScope, DsOliEventConfig.Kind),
        DsOliXMLHelper.getTagValue(varEnterScope, DsOliEventConfig.Type),
        DsOliXMLHelper.parseLong(address, HexBase))
    } else {
      throw new Exception("DsOliEventParser::vESEvent: No node found for : " + DsOliEventConfig.VariableEnterScope + " in event : " + event)
    }
  }

  /**
   * Parse the variable leaving scope event
   *
   * @param id the event id
   * @param event the event node
   * @return an instance of the event object
   */
  def vLSEvent(id: Int, event: Node): DsOliEvent = {
    // Get the variable-left-scope node
    val varLeftScopeOpt = DsOliXMLHelper.getTagNode(event, DsOliEventConfig.VariableLeftScope)

    return if (varLeftScopeOpt.isDefined) {
      val varLeftScope = varLeftScopeOpt.get
      DsOliLogger.debug("DsOliEventParser::vLSEvent: varLeftScope: " + varLeftScope)
      val address = convertAddressString(DsOliXMLHelper.getTagValue(varLeftScope, DsOliEventConfig.Address))
      DsOliLogger.debug("DsOliEventParser::vLSEvent: address: " + address)
      new DsOliVLSEvent(
        id,
        getSourceLocation(Some(event)),
        DsOliXMLHelper.getTagValue(varLeftScope, DsOliEventConfig.Name),
        DsOliXMLHelper.parseLong(address, HexBase))
    } else {
      throw new Exception("DsOliEventParser::vLSEvent: No node found for : " + DsOliEventConfig.VariableLeftScope + " in event : " + event)
    }
  }

  /**
   * Parse the free event
   *
   * @param id the event id
   * @param event the event node
   * @return an instance of the event object
   */
  def fEvent(id: Int, event: Node): DsOliEvent = {
    val funSignature = classSignature + "fEvent:"
    // Get the free node
    var freeOpt = DsOliXMLHelper.getTagNode(event, DsOliEventConfig.Free)

    return if (freeOpt.isDefined) {
      val free = freeOpt.get
      DsOliLogger.debug(funSignature + "free xml tag: " + free)
      val event = new DsOliFreeEvent(
        id,
        getSourceLocation(DsOliXMLHelper.getTagNode(free, DsOliEventConfig.SourceLocation)),
        DsOliXMLHelper.getTagValue(free, DsOliEventConfig.ArgCodeFragment),
        DsOliXMLHelper.parseLong(convertAddressString(DsOliXMLHelper.getTagValue(free, DsOliEventConfig.ArgValue)), HexBase))
      DsOliLogger.debug(funSignature + "new free event created: " + event)
      event
    } else {
      throw new Exception("DsOliEventParser::fEvent: No node found for : " + DsOliEventConfig.Free + " in event : " + event)
    }
  }

  /**
   * Parse the XML event trace
   *
   * @return a DsOliEvents instance, containing all events
   */
  override def parse(): DsOliEvents = {
    val funSignature = classSignature + "parse: "

    // Create a variable buffer
    val eventList = ListBuffer[DsOliEvent]()

    // Load the XML validated against the XSD
    val factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    val xsdStream = new FileInputStream(new File(xsdFilePath))
    val schema = factory.newSchema(new StreamSource(xsdStream))
    DsOliLogger.verbose("DsOliEventParser::parse: xsd done")

    val source = new FileInputStream(new File(xmlFilePath))
    val xml = new XMLXSDLoader(schema).load(source)

    var timeStep = 0

    // Parse the events
    val events = DsOliXMLHelper.getTagSeq(xml, DsOliEventConfig.Event)
    events.foreach { event =>
      // Fetch the id of the event
      val id = DsOliXMLHelper.getTagAttribute(event, DsOliEventConfig.Id).toInt
      DsOliLogger.debug(funSignature + "#Event " + id + "# parse event " + id)

      val firstChildOpt = DsOliXMLHelper.getTagNode(event, "_")
      if (firstChildOpt.isDefined) {
        val firstChild = firstChildOpt.get
        val eventResult = firstChild.label match {
          case DsOliEventConfig.OperationTransition => oTEvent(id, event)
          case DsOliEventConfig.BlockTransition => bTEvent(id, event)
          case DsOliEventConfig.MemoryWrite => mWEvent(id, event)
          case DsOliEventConfig.VariableEnterScope => vESEvent(id, event)
          case DsOliEventConfig.VariableLeftScope => vLSEvent(id, event)
          case DsOliEventConfig.Free => fEvent(id, event)
          case _ => new DsOliCommentEvent(id, getSourceLocation(Some(event)), event.text)
        }
        eventList.append(eventResult)
      } else {
        throw new Exception("DsOliEventParser::fEvent: No entry node found in event : " + event)
      }

      print("Event: " + timeStep + "/" + events.size + "\r")
      timeStep += 1
      DsOliLogger.debug(funSignature + "#Done Event " + id + "# parse event " + id)
    }

    // Setup up the events
    val eventArray = new Array[DsOliEvent](eventList.size)
    eventList.copyToArray(eventArray)
    val dsOliEvents = new DsOliEvents(eventArray)

    return dsOliEvents
  }
}

object DsOliEventParser {
  /**
   * Convenience tuple for (address, offset)
   */
  type AddressOffsetTuple = (Long, Long)
}