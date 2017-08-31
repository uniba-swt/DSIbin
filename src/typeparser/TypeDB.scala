
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
 * TypeDB.scala created on Oct 16, 2014
 *
 * Description: Main type lookup for DSI
 */
package typeparser

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import extlogger.DsOliLogger
import pointstograph.FieldType._
import scala.collection.mutable.HashSet
import pointstograph.ITypeDB
import pointstograph.DsOliType
import pointstograph.DsOliVertexField
import pointstograph.DsOliType
import boxcalculation.DsOliCell
import pointstograph.DsOliVertexMemory
import boxcalculation.DsOliBox
import boxcalculation.DsOliBoxStep
/**
 * @author DSI
 *
 */
class TypeDB extends ITypeDB {

  // Mapping between sugared and de-sugared type names
  val deSugarLookup = new HashMap[String, String]()
  // Storage for type names
  val typeLookup = new HashSet[String]()
  // Mapping for type and type size
  val typeSizes = new HashMap[String, Int]()
  // In case of more information, add here: typeID: String, isStack: Boolean, offset : Long
  val typeInformation = new HashMap[String, (String, Boolean, Long)]
  // Mapping between type and struct fields
  val structFields = new HashMap[String, ListBuffer[DsOliVertexField]]()
  // Cache for speed improvements
  val typeOffsetCompoundStore = new HashMap[(String, Long), ListBuffer[DsOliType]]

  val classSignature = "TypeDB::"

  // Load some default types
  putType("VOID")
  putTypeSize("VOID", 8)

  putType("INT32")
  putTypeSize("INT32", 4)

  putType("INT8")
  putTypeSize("INT8", 1)

  /**
   * Save the sugared and de-sugared type names
   * 
   * @param sugaredType the sugared type
   * @param deSugaredType the de-sugared type
   * @return Boolean if the types could be saved
   */
  def putSugaredToDesugared(sugaredType: String, deSugaredType: String): Boolean = {
    val funSignature = classSignature + "putSugaredToDesugared: "
    DsOliLogger.debug(funSignature + "entered: ")
    return if (deSugarLookup.contains(sugaredType)) {
      deSugarLookup.get(sugaredType) == deSugaredType
    } else {
      // For convenience place the de-sugared type into the type lookup
      this.putType(deSugaredType)
      deSugarLookup.put(sugaredType, deSugaredType).isDefined
    }
  }

  /**
   * Save the type. Always stores the pointer and double-pointer versions as well
   * 
   * @param typeString the type name to store
   * @return Boolean if the type could be saved
   */
  def putType(typeString: String): Boolean = {
    val funSignature = classSignature + "putType: "
    DsOliLogger.debug(funSignature + "entered: ")
    return if (!typeLookup.contains(typeString)) {
      val retVal = typeLookup.add(typeString)
      if (!typeSizes.contains(typeString + " *")) {
        this.putTypeSize(typeString + " *", 8)
      }
      if (!typeSizes.contains(typeString + " **")) {
        this.putTypeSize(typeString + " **", 8)
      }
      retVal
    } else {
      true
    }
  }

  /**
   *  Save the size for a type.
   *  
   *  @param typeString the type name
   *  @param size the type size
   * @return Boolean if the type size could be saved
   */
  def putTypeSize(typeString: String, size: Int): Boolean = {
    val funSignature = classSignature + "putTypeSize: "
    DsOliLogger.debug(funSignature + "entered: ")
    return if (typeSizes.contains(typeString)) {
      typeSizes.get(typeString).get == size
    } else {
      typeSizes.put(typeString, size).isDefined
    }
  }

  /**
   * Save meta-data for type.
   * 
   * @param typeString the type name
   * @param typeID the type ID provided by the external instrumentation framework
   * @param isStack flag if type is from heap or stack
   * @param offsetFromStart where the type actually starts from (important if type is on the stack)
   * @return Boolean if the information could be saved
   */
  def putTypeIdentification(typeString: String, typeID: String, isStack: Boolean, offsetFromStart: Long): Boolean = {
    val funSignature = classSignature + "putTypeIdentification: "
    DsOliLogger.debug(funSignature + "entered: ")
    return if (typeInformation.contains(typeString)) {
      val (typeIDPresent, isStackPresent, offsetFromStartPresent) = typeInformation.get(typeString).get
      typeIDPresent == typeID && isStackPresent == isStack && offsetFromStartPresent == offsetFromStart
    } else {
      typeInformation.put(typeString, (typeID, isStack, offsetFromStart)).isDefined
    }
  }

  /**
   * Fetch the meta-data information for a type
   * 
   * @param typeString the type name
   * @return Option tuple of (type id, is stack, offset from start)
   */
  def getTypeIdentification(typeString: String): Option[(String, Boolean, Long)] = {
    typeInformation.get(typeString)
  }

  /**
   * Save all the fields of a struct
   * 
   * @param typeString the type name
   * @param fieldsd the list of struct fields of that type
   * @return Boolean if the fields could be saved
   */
  def putStructFields(typeString: String, fields: ListBuffer[DsOliVertexField]): Boolean = {
    val funSignature = classSignature + "putStructFields: "
    DsOliLogger.debug(funSignature + "entered: ")
    return if (!structFields.contains(typeString)) {
      structFields.put(typeString, fields).isDefined
    } else {
      false
    }
  }

  override def toString(): String = {
    val funSignature = classSignature + "toString: "
    DsOliLogger.debug(funSignature + "entered: ")
    var retString = "Sugar -> Desugar: \n"
    this.deSugarLookup.foreach(
      _ match {
        case (k, v) =>
          retString += "\t" + k + "->" + v + "\n"
      })
    retString += "Type lookup: \n"
    this.typeLookup.foreach {
      retString += "\t" + _ + "\n"
    }
    retString += "Type sizes: \n"
    this.typeSizes.foreach {
      _ match {
        case (k, v) =>
          retString += "\t" + k + "->" + v + "\n"
      }
    }
    retString += "Struct fields: \n"
    this.structFields.foreach {
      _ match {
        case (k, v) =>
          retString += "\t" + k + "->\n"
          v.foreach {
            retString += "\t\t" + _ + "\n"
          }
      }
    }
    return retString
  }

  /**
   * Get a type object for a type string
   * 
   * @param typeString the type name
   * @return Option instance of the type object
   */
  def getTypeObject(typeString: String): Option[DsOliType] = {
    val funSignature = classSignature + "getTypeObject: "
    DsOliLogger.debug(funSignature + "entered: ")
    DsOliLogger.debug("TypeDB::getTypeObject: entered. typeString: " + typeString)
    return if (typeSizes.contains(typeString) && structFields.contains(typeString)) {
      Some(new DsOliType(structFields.get(typeString).get,
        typeString,
        typeSizes.get(typeString).get))
    } else if (typeSizes.contains(typeString) && typeLookup.contains(typeString)) {
      Some(new DsOliType(null,
        typeString,
        typeSizes.get(typeString).get))
    } else if (typeString.contains("*")) {
      if (typeSizes.get(typeString).isEmpty) {
        throw new Exception("TypeDB::getTypeObject: (*) unknown type: " + typeString + ".")
      }
      Some(new DsOliType(null,
        typeString,
        typeSizes.get(typeString).get))
    } else if (typeSizes.contains(typeString)) {
      Some(new DsOliType(null,
        typeString,
        typeSizes.get(typeString).get))
    } else {
      throw new Exception("TypeDB::getTypeObject: unknown type: " + typeString + ".")
    }
  }

  /**
   * Get the size of type in bytes
   * 
   * @param typeString the type
   * @return Option size in bytes
   */
  def getSizeInBytes(typeString: String): Option[Int] = {
    val funSignature = classSignature + "getSizeInBytes: "
    DsOliLogger.debug(funSignature + "entered: ")
    return typeSizes.get(typeString)
  }

  /**
   * Checks, if offset is actually at type boundaries, i.e., compound(s) exist at this
   * offset
   * 
   * @param offset the offset inside of the type
   * @param typeString the type
   * @return Option list of compound types residing at this offset
   */
  def isOffsetAtTypeBoundary(offset: Long, typeString: String): Option[ListBuffer[DsOliType]] = {
    val funSignature = classSignature + "isOffsetAtTypeBoundary: "
    DsOliLogger.debug(funSignature + "entered: ")
    DsOliLogger.debug("TypeDB::isOffsetAtTypeBoundary: Entered. offset: " + offset + ", typeString: " + typeString)

    // Shortcut for speed improvement
    val keyOpt = typeOffsetCompoundStore.keys.find {
      keyTuple =>
        val (keyStr, keyOff) = keyTuple
        keyStr == typeString && keyOff == offset
    }
    if (keyOpt.isDefined) {
      return typeOffsetCompoundStore.get(keyOpt.get)
    }

    // Lookup compounds
    val retCompoundsOpt = collectCompoundsForOffset(offset, typeString)
    return if (retCompoundsOpt.isDefined) {
      val retCompounds = retCompoundsOpt.get
      val compList = retCompounds.foldLeft[ListBuffer[DsOliType]](new ListBuffer[DsOliType]()) {
        (hist, cur) =>
          hist.append(cur._2)
          hist
      }
      // Do bookkeeping for speed improvements
      typeOffsetCompoundStore.put((typeString, offset), compList)
      Some(compList)
    } else {
      None
    }
  }

  /**
   * Fetch the type at a particular offset in a type
   * 
   * @param offset the offset
   * @param typeString the type
   * @return Option of the type
   */
  def getTypeAtOffset(offset: Long, typeString: String): Option[DsOliType] = {
    val funSignature = classSignature + "getTypeAtOffset: "
    DsOliLogger.debug(funSignature + "entered: ")
    // Fetch all elements with this offset
    val types = this.structFields.get(typeString).get.filter(_.vOffset == offset)
    // There must only be one element with this offset
    if (types.length == 1) {
      // Get the sub type string and create a type object from it
      val subTypeString = types.head.cType
      this.getTypeObject(subTypeString)
    } else {
      None
    }
  }

  /**
   * Fetch the field name of a type at a particular offset in a type
   * 
   * @param offset the offset
   * @param typeString the type
   * @return Option of the field name
   */
  def getFieldNameAtOffset(offset: Long, typeString: String): Option[String] = {
    val funSignature = classSignature + "getFieldNameAtOffset: "
    DsOliLogger.debug(funSignature + "entered: ")
    // Fetch all elements with this offset
    val types = this.structFields.get(typeString).get.filter(_.vOffset == offset)
    // There must only be one element with this offset
    if (types.length == 1) {
      // Get the sub type string and create a type object from it
      Some(types.head.name)
    } else {
      None
    }
  }

  /**
   * Checks, if the source type can be found at the target
   * 
   * @param targetTypeObj the target type object
   * @param sourceType the source type
   * @return true or false
   */
  def hasMatchingTypeAtStart(targetTypeObj: DsOliType, sourceType: String): Boolean = {
    val funSignature = classSignature + "hasMatchingTypeAtStart: "
    DsOliLogger.debug(funSignature + "entered: targetTypeObj: " + targetTypeObj + " sourceType: " + sourceType)
    // Check the first element of the target
    if (targetTypeObj.fields.head.cType == sourceType) {
      DsOliLogger.debug(funSignature + "cType == sourceType: " + targetTypeObj.fields.head.cType + " == " + sourceType)
      return true
      // If the first element of the target is a compound, recurse with the type of the first element
    } else if (targetTypeObj.fields.head.fType == Compound) {
      DsOliLogger.debug(funSignature + "fType == Compound: recurse")
      hasMatchingTypeAtStart(this.getTypeObject(targetTypeObj.fields.head.cType).get, sourceType)
    } else {
      DsOliLogger.debug(funSignature + "no match")
      false
    }
  }

  /**
   * Fetches all types for a cell
   * 
   * @param cellA the cell which defines the start/end address
   * @param typeObj the object type
   * @param startAddr where to start
   * @param retList results are placed in here (field, start address, size)
   *
   */
  def drillDownTypes(cellA: DsOliCell, typeObj: DsOliType, startAddr: Long, retList: ListBuffer[(DsOliType, Long, Long)]): Unit = {
    val funSignature = classSignature + "drillDownTypes:"
    DsOliLogger.debug(funSignature + "entered: " + cellA + "; typeObj: " + typeObj)

    // Check through all fields of this type object
    typeObj.fields.foreach {
      field =>

        // Check for compound and if we are currently within this particular compound
        if (field.fType == Compound &&
          startAddr + field.vOffset <= cellA.bAddr &&
          startAddr + field.vOffset + field.vSize >= cellA.eAddr) {

          DsOliLogger.debug(funSignature + "found match, recurse: " + field)

          // Record the match
          retList.append((getTypeObject(field.cType).get, startAddr + field.vOffset, field.vSize))

          // Recurse
          drillDownTypes(cellA, getTypeObject(field.cType).get, startAddr + field.vOffset, retList)
        }
    }
  }

  /**
   * Expand the cell type outwards to find a match
   * 
   * @param vertex the vertex we are searching in
   * @param cell the cell we are considering
   * @param boxB the strand to check
   * @param boxStep the set of boxes for a particular time step (contains cycles)
   * @return Option tuple of (start address, type object, cell)
   */
  def expandCellTypeOutwards(vertex: DsOliVertexMemory, cellA: DsOliCell, boxB: DsOliBox, boxStep: DsOliBoxStep): Option[(Long, DsOliType, DsOliCell)] = {

    val funSignature = classSignature + "expandCellTypeOutwards:"
    DsOliLogger.debug(funSignature + "entered: vertex = " + vertex + ", cellA = " + cellA + " boxB = " + boxB)

    // Short cut test: are the two cells actually the same
    val sameCellOpt = boxB.cells.find(cellB => cellB == cellA)
    if (sameCellOpt.isDefined) {
      DsOliLogger.debug(funSignature + "linear shortcut found")
      return Some((0, cellA.cType, sameCellOpt.get))
    }
    if (boxB.cycleId != 0) {
      val sameCellCyclicOpt = boxStep.cycles.get(boxB.cycleId).get.cells.find(cellB => cellB == cellA)
      if (sameCellCyclicOpt.isDefined) {
        DsOliLogger.debug(funSignature + "cyclic shortcut found")
        return Some((0, cellA.cType, sameCellCyclicOpt.get))
      }
    }

    // Now expand
    val enclosingCellAOpt = vertex.vType.fields.find {
      field =>
        field.fType == Compound &&
          vertex.bAddr + field.vOffset <= cellA.bAddr &&
          vertex.bAddr + field.vOffset + field.vSize >= cellA.eAddr
    }

    val retTypes = new ListBuffer[(DsOliType, Long, Long)]

    // Always include the outermost context i.e. the vertex
    retTypes.append((vertex.vType, vertex.bAddr, vertex.vType.size))

    // Only try to drill down, if we got an enclosing match (i.e. not operating on vertex level)
    if (enclosingCellAOpt.isDefined) {
      val enclosingTypes = new ListBuffer[DsOliVertexField]()
      enclosingTypes.+=(enclosingCellAOpt.get)
      val field = enclosingCellAOpt.get
      // Add the current context
      retTypes.append((getTypeObject(field.cType).get, vertex.bAddr + field.vOffset, field.vSize))

      drillDownTypes(cellA, getTypeObject(enclosingCellAOpt.get.cType).get, vertex.bAddr + enclosingCellAOpt.get.vOffset, retTypes)
      DsOliLogger.debug(funSignature + "retTypes: " + retTypes)
    }

    // Iterate backwards over found types and see if we actually got an enclosing match
    retTypes.reverse.foreach {
      retType =>
        DsOliLogger.debug(funSignature + "processing retType: " + retType)
        val (field, startAddr, size) = retType
        DsOliLogger.debug(funSignature + "startAddr, size: " + startAddr + "," + size)
        var enclosedCells = boxB.cells.filter(cellB => startAddr <= cellB.bAddr && startAddr + size >= cellB.eAddr)

        // Test for cyclic strand
        if (boxB.cycleId != 0) {
          DsOliLogger.debug(funSignature + "boxB is cyclic adding cells.")
          enclosedCells.appendAll(boxStep.cycles.get(boxB.cycleId).get.cells.filter(cellB => startAddr <= cellB.bAddr && startAddr + size >= cellB.eAddr))
        }
        DsOliLogger.debug(funSignature + "enclosedCells: " + enclosedCells)
        if (enclosedCells.size == 1) {
          DsOliLogger.debug(funSignature + "we found exactly one cell in the enclosing type (" + field + ") => Success")
          return Some((startAddr, field, enclosedCells.head))
        } else if (enclosedCells.size > 1) {
          DsOliLogger.debug(funSignature + "we found more than one cell in the enclosing type (" + field + ") => Stopping")
          return None
        } // else: size == 0 => continue
    }

    DsOliLogger.debug(funSignature + "we found no suitable match. Stopping")
    None
  }

  /**
   * Collect all compounds for a particular offset
   * 
   * @param offset the offset
   * @param typeString the type
   * @return Option list of (offset, type object)
   */
  def collectCompoundsForOffset(offset: Long, typeString: String): Option[ListBuffer[(Long, DsOliType)]] = {
    val funSignature = classSignature + "collectCompoundsForOffset: "
    DsOliLogger.debug(funSignature + "entered: offset: " + offset + " typeString: " + typeString)
    val retList = new ListBuffer[(Long, DsOliType)]
    val structFieldsOpt = this.structFields.get(typeString)
    if (structFieldsOpt.isDefined) {
      DsOliLogger.debug(funSignature + "is compound field")
      val structFields = structFieldsOpt.get
      val typeObjectOpt = this.getTypeObject(typeString)
      if (typeObjectOpt.isDefined) {
        val typeObject = typeObjectOpt.get
        DsOliLogger.debug(funSignature + "typeObject is defined: " + typeObject)
        // Append the enclosing type first
        if (typeObject.size > offset) {
          retList.append((offset, typeObject))
        }
        structFields.foreach {
          structField =>
            DsOliLogger.debug(funSignature + "inspecting struct field: " + structField)
            if (structField.fType == Compound && structField.vOffset <= offset && offset <= structField.vOffset + structField.vSize) {
              DsOliLogger.debug(funSignature + "offset is inside of compound: " + structField)
              val ret = collectCompoundsForOffset(offset - structField.vOffset, structField.cType)
              if (ret.isDefined) {
                DsOliLogger.debug(funSignature + "recursion result: " + ret.get)
                DsOliLogger.debug(funSignature + "current result: " + retList)
                retList.appendAll(ret.get)
              } else {
                DsOliLogger.debug(funSignature + "recursion result empty")
              }
            } else {
              DsOliLogger.debug(funSignature + "either no compound, or offset outside of range: " + offset + ", vOffset " + structField.vOffset + ", vSize " + structField.vSize)
            }
        }
      } else {
        DsOliLogger.debug(funSignature + "typeObject not defined: " + typeString)
      }
    } else {
      DsOliLogger.debug(funSignature + "is no compound field: " + typeString)
    }
    if (retList.length > 0) Some(retList) else None
  }

  /**
   *  Fetch the matching type for a given offset
   *  
   *  @param offset the offset
   *  @param typeString the type
   *  @param subTypes list of all sub types to match against
   *  @return Option tuple for matched type (offset, type object)
   */
  def getMatchingTypeForOffset(offset: Long, typeString: String, subTypes: ListBuffer[DsOliType]): Option[(Long, DsOliType)] = {
    val funSignature = classSignature + "getMatchingTypeForOffset: "
    DsOliLogger.debug(funSignature + "entered: ")

    DsOliLogger.debug(funSignature + "Entered. offset: " + offset + " typeString: " + typeString + " subTypeString: " + subTypes)
    val typeObjectOpt = this.getTypeObject(typeString)

    if (typeObjectOpt.isDefined) {

      DsOliLogger.debug(funSignature + "typeObjectOpt.isDefined")
      val sourceTypesOpt = collectCompoundsForOffset(offset, typeString)

      if (sourceTypesOpt.isDefined) {
        DsOliLogger.debug(funSignature + "sourceTypesOpt.isDefined")
        // Start out from the inner most
        val sourceTypes = sourceTypesOpt.get.reverse
        val targetTypes = subTypes.reverse
        DsOliLogger.debug(funSignature + "sourceType: " + sourceTypes)
        DsOliLogger.debug(funSignature + "targetType: " + targetTypes)
        val matchedTypeOpt = sourceTypes.find { sourceTypeTuple =>
          val (_, sourceType) = sourceTypeTuple
          targetTypes.exists {
            targetType =>
              DsOliLogger.debug(funSignature + " comparing: sourceType == targetType: " + sourceType + " == " + targetType)
              sourceType == targetType
          }
        }
        if (matchedTypeOpt.isDefined) {
          DsOliLogger.debug(funSignature + "found matchedType: offset: " + matchedTypeOpt.get._1 + ", type: " + matchedTypeOpt.get._2)
          Some(matchedTypeOpt.get)
        } else {
          DsOliLogger.debug(funSignature + "no type match")
          return None
        }
      } else {
        DsOliLogger.debug(funSignature + "no source types available: " + offset + ", " + typeString)
        None
      }
    } else {
      DsOliLogger.debug(funSignature + "no source type found: " + typeString)
      None
    }
  }

}