
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
 * ITypeDB.scala created on Oct 16, 2014
 *
 * Description: Interface for DSI's type DB for looking up type information
 */
package pointstograph

import scala.collection.mutable.ListBuffer
import boxcalculation.DsOliCell
import boxcalculation.DsOliBox
import boxcalculation.DsOliBoxStep

/**
 * @author DSI
 *
 */
trait ITypeDB {
  def getTypeObject(typeString: String): Option[DsOliType]
  def getSizeInBytes(typeString: String): Option[Int]
  def isOffsetAtTypeBoundary(offset: Long, typeString: String): Option[ListBuffer[DsOliType]]
  def getTypeAtOffset(offset: Long, typeString : String) : Option[DsOliType]
  def getFieldNameAtOffset(offset: Long, typeString :String): Option[String]
  def getMatchingTypeForOffset(offset: Long, typeString: String, subTypes: ListBuffer[DsOliType]): Option[(Long, DsOliType)]

  def putSugaredToDesugared(sugaredType: String, deSugaredType: String): Boolean
  def putType(typeString: String): Boolean
  def putTypeSize(typeString: String, size: Int): Boolean
  def putStructFields(typeString: String, fields: ListBuffer[DsOliVertexField]): Boolean

  def expandCellTypeOutwards(vertex: DsOliVertexMemory, cellA: DsOliCell, boxB: DsOliBox, boxStep: DsOliBoxStep): Option[(Long, DsOliType, DsOliCell)]

  def getTypeIdentification(typeString: String): Option[(String, Boolean, Long)]
  def putTypeIdentification(typeString: String, typeID: String, isStack: Boolean, offsetFromStart : Long): Boolean
}