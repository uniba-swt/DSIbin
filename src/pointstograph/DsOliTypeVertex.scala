
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
 * DsOliTypeVertex.scala created on Sep 5, 2016
 *
 * Description: A type vertex used by the merged type graph for DSIbin
 */
package pointstograph

import scala.collection.mutable.ListBuffer
import binarytypes.DSIType
import scala.collection.mutable.HashMap

/**
 * @author DSI
 *
 */
class DsOliTypeVertex(
  bAddr: Long,
  eAddr: Long,
  vType: DsOliType,
  id: Long,
  isStack: Boolean = false,
  val typeID : String,
  val offsetFromStart : Long,
  val dsiTypeInstances: ListBuffer[(DSIType, Long)],
  val typeMatrix: HashMap[(DSIType, DSIType), Boolean]) extends DsOliVertexMemory(bAddr, eAddr, vType, id, isStack) {

  def this(bAddr: Long, eAddr: Long, vType: DsOliType, id: Long,
    isStack: Boolean, typeID : String, offsetFromStart : Long, dsiTypeInstances: ListBuffer[(DSIType, Long)]) =
    this(bAddr, eAddr, vType, id, isStack, typeID, offsetFromStart, dsiTypeInstances, new HashMap[(DSIType, DSIType), Boolean])

  def addTypeCompatibility(firstDSIType : DSIType, secondDSIType : DSIType, compatible : Boolean) = {
    if(!typeMatrix.contains((firstDSIType, secondDSIType))){
      typeMatrix.put((firstDSIType, secondDSIType),true)
    }
    val typeCompatible = typeMatrix.get((firstDSIType, secondDSIType)).get
    typeMatrix.put((firstDSIType, secondDSIType), typeCompatible && compatible)
  }
  
  def getSize() : Long = {
    eAddr - bAddr 
  }

  override def toString(): String = {
    "[DsOliTypeVertex: " + "id=" + id + ", bAddr=" + bAddr+ ", eAddr=" + eAddr + ", vType: " + vType
  }
}