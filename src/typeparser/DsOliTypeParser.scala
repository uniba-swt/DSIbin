
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
 * DsOliTypeParser.scala created on Nov 4, 2014
 *
 * Description: Parse the XML information and
 * store the information into the type DB
 * used by DSI
 */
package typeparser

import pointstograph.ITypeDB
import xmltools.DsOliXMLHelper
import scala.xml.Node
import extlogger.DsOliLogger
import pointstograph.DsOliVertexField
import scala.collection.mutable.ListBuffer
import pointstograph.FieldType._
import java.io.FileInputStream
import java.io.File
import xmltools.XMLXSDLoader

/**
 * @author DSI
 *
 */
class DsOliTypeParser(xmlFilePath: String) extends IDsOliTypeParser {

  def processAlias(typeNode: Node, typeDB: ITypeDB): Unit = {
    // Currently not required
  }

  /**
   * Process each field of a compound and store it into the type DB
   *
   * @param compound the XML node
   * @param structType name of the struct type
   * @param typeDB the type DB used by DSI
   */
  def processFields(compound: Node, structType: String, typeDB: ITypeDB): Unit = {
    val fieldsNodeOpt = DsOliXMLHelper.getTagNode(compound, DsOliTypeConfig.Fields)

    if (fieldsNodeOpt.isEmpty) return

    val fields = DsOliXMLHelper.getTagSeq(fieldsNodeOpt.get, DsOliTypeConfig.Field)

    if (fields.length > 0) {
      val fieldList = new ListBuffer[DsOliVertexField]()
      fields.foreach {
        field =>
          DsOliLogger.debug("DsOliTypeParser::processFields: field: " + field)
          val fieldName = DsOliXMLHelper.getTagValue(field, DsOliTypeConfig.Name)
          val fieldSugaredType = DsOliXMLHelper.getTagValue(field, DsOliTypeConfig.SugaredType)
          val fieldDesugaredType = DsOliXMLHelper.getTagValue(field, DsOliTypeConfig.DesugaredType)
          val sizeInBitsStr = DsOliXMLHelper.getTagValue(field, DsOliTypeConfig.SizeInBits)
          val offsetInBytesStr = DsOliXMLHelper.getTagValue(field, DsOliTypeConfig.OffsetInBytes)

          val sizeInBytes = sizeInBitsStr.toInt / 8
          val offsetInBytes = offsetInBytesStr.toInt

          // Determine the field type
          val fType = if (fieldDesugaredType.contains("*")) {
            Pointer
          } else if (fieldDesugaredType.contains(DsOliTypeConfig.Struct) || fieldDesugaredType.contains(DsOliTypeConfig.Union)) {
            Compound
          } else {
            Data
          }

          fieldList.append(new DsOliVertexField(fieldName, fieldDesugaredType, sizeInBytes, offsetInBytes, fType))

          typeDB.putType(fieldDesugaredType)
          typeDB.putTypeSize(fieldDesugaredType, sizeInBytes)

          if ((fieldSugaredType != "") && (fieldSugaredType != fieldDesugaredType)) {
            typeDB.putSugaredToDesugared(fieldSugaredType, fieldDesugaredType)
          }
      }

      if (fieldList.length > 0) {
        typeDB.putStructFields(structType, fieldList)
      }
    }
  }

  /**
   * Parse the XML type node and store it into the type DB
   *
   * @param typeNode the type of the node
   * @param typeDB the type DB used by DSI
   *
   */
  def processCompound(typeNode: Node, typeDB: ITypeDB): Unit = {
    val compound = typeNode
    val mode = DsOliXMLHelper.getTagAttribute(compound, DsOliTypeConfig.Mode)
    DsOliLogger.debug("DsOliTypeParser::processCompound: compound = " + compound)
    if (mode == DsOliTypeConfig.Struct) {
      val structName = DsOliXMLHelper.getTagValue(compound, DsOliTypeConfig.TagName)
      val structSizeStr = DsOliXMLHelper.getTagValue(compound, DsOliTypeConfig.SizeInBytes)
      // Get information where the type is allocated stack/heap and the 
      // unique id for this type: heap -> allocation site; stack -> function start addr
      val isStack = DsOliXMLHelper.getTagValue(compound, DsOliTypeConfig.IsStack)
      val typeID = DsOliXMLHelper.getTagValue(compound, DsOliTypeConfig.TypeID)
      // Get the offset which should only be non-zero in case of the stack or nested structs in a heap object. 
      // Stack offset should be considered as negative
      val offsetFromStart = DsOliXMLHelper.getTagValue(compound, DsOliTypeConfig.OffsetFromStart)

      val structType = DsOliTypeConfig.Struct + " " + structName
      val structSize = DsOliXMLHelper.parseInt(structSizeStr, 10)
      typeDB.putType(structType)
      typeDB.putTypeSize(structType, structSize)
      typeDB.putTypeIdentification(structType, typeID, if (isStack == "0") false else true, offsetFromStart.toLong)

      // Currently skip the location: DsOliTypeConfig.Location

      // Parse all the fields of the compound
      val fieldsOpt = processFields(compound, structType, typeDB)

    } else {
      DsOliLogger.warning("DsOliTypeParser::processCompound: mode unknown: " + mode)
    }
  }

  /**
   * Parse the types XML file created by the external instrumentation framework
   *
   * @return a new TypeDB instance holding the type information
   */
  def parseTypes(): ITypeDB = {

    val xml = scala.xml.XML.loadFile(this.xmlFilePath)
    DsOliLogger.debug("DsOliTypeParser::parseTypes: entered. xml: " + xml)

    // TypeDB holds the type information 
    val typeDB: ITypeDB = new TypeDB()

    DsOliLogger.debug("DsOliTypeParser::parseTypes: types: " + types)

    // Cycle through all types
    val types = DsOliXMLHelper.getTagSeq(xml, "_")
    types.foreach { typeNode =>
      typeNode.label match {
        // Type alias 
        case DsOliTypeConfig.Alias =>
          DsOliLogger.debug("DsOliTypeParser::parseTypes: alias found")
          processAlias(typeNode, typeDB)

        // Compound type
        case DsOliTypeConfig.Compound =>
          DsOliLogger.debug("DsOliTypeParser::parseTypes: compound found")
          processCompound(typeNode, typeDB)

        case _ =>
          throw new Exception("DsOliTypeParser::parseTypes: Unknown label : " + typeNode.label)

      }
    }
    DsOliLogger.debug("DsOliTypeParser::parseTypes: " + typeDB)
    return typeDB
  }
}