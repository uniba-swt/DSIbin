
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
 * DSIType.scala created on Aug 30, 2016
 *
 * Description: Adjusted DSIType tailored for DSIbin, e.g., the groupId for
 * indicating types that are spawned due to mapping abmiguity.
 */
package binarytypes

import scala.collection.mutable.ListBuffer
import binarytypes.DSIType._
import pointstograph.DsOliVertexField
import pointstograph.DsOliVertexField

/**
 * @author DSI
 *
 */

class DSIType(
  val id: DSITypeId,
  val primitiveTypes: ListBuffer[DsOliVertexField],
  val linkageOffset: Long,
  val size: Long,
  val byteSize: Long,
  var groupId: DSITypeId) {
  
  val nestedTypeInstances = new ListBuffer[(DSIType, Long)]
  var isNested = false

  def this(primitiveTypes: ListBuffer[DsOliVertexField], linkageOffset: Long) =
    this(DSIType.getId, primitiveTypes, linkageOffset, primitiveTypes.length,
      // size in bytes: sum all offsets (!) plus the size (!) of the last element
      primitiveTypes.foldLeft(0)((sum, item) => sum + (item.vOffset - sum)) + primitiveTypes.last.vSize,
      DSIType.getCurId)

  override def toString(): String = {
    "[DSIType: " + "id=" + id + ", gId=" + groupId + " lOff=" + linkageOffset + ", primTypes(" + primitiveTypes.size + ")=" + primitiveTypes + "]"
  }
  
  def deepCopy() : DSIType = {
    new DSIType(this.id, this.primitiveTypes, this.linkageOffset, this.size, this.byteSize, this.groupId)
  }
}

object DSIType {

  type DSITypeId = Long
  // Important to start from 1, as zero means 0 reference
  var id: DSITypeId = 1
  def getId(): Long = {
    val retId = id;
    id += 1
    return retId
  }

  def getCurId(): Long = {
    return id
  }
  
  def primitiveTypesByteLen(primitiveTypes: ListBuffer[DsOliVertexField]) : Long = {
   primitiveTypes.foldLeft(0)((sum, item) => sum + item.vSize ) 
  }
}

