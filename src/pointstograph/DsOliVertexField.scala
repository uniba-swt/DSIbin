
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
 * VertexField.scala created on Oct 6, 2014
 *
 * Description: A field of a typed vertex
 */
package pointstograph

import FieldType._
import extlogger.DsOliLogger

/**
 * @author DSI
 * @param name the name of the field, i.e. variable name
 * @param cType the [C] type, e.g., INT
 * @param vSize the size of the field
 * @param vOffset the offset within the vertex
 * @param fType the field type
 */
class DsOliVertexField(
  val name: String,
  val cType: String,
  val vSize: Int,
  val vOffset: Int,
  val fType: FieldType) {
  override def toString(): String = {
    "[name = " + name + "]" +
      "[cType = " + cType + ", vSize = " + vSize + ", vOffset = " + vOffset + ", fType = " + fType + "]"
  }
  
  override def equals(other: Any): Boolean = {
    return other match {
      case that: DsOliVertexField =>
        DsOliLogger.debug("DsOliVertexField::equals: called on element " + this + " with " + that)
        this.name == that.name && this.cType == that.cType && this.vSize == that.vSize && this.fType == that.fType
      case _ =>
        DsOliLogger.debug("DsOliVertexField::equals: called on wrong element ")
        false
    }
  }
}

