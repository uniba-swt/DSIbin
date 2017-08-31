
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
 * VertexType.scala created on Oct 6, 2014
 *
 * Description: The representation of a type within DSI
 */
package pointstograph

import scala.collection.mutable.ListBuffer
import extlogger.DsOliLogger

/**
 * @author DSI
<<<<<<< HEAD
 * @param fields the fields of the typed vertex
 * @param vType the type string
 * @param size the size in bytes
=======
>>>>>>> 686dc2fc4be4e3ddf314a5576b76fcc11e873b80
 *
 */
class DsOliType(
  val fields: ListBuffer[DsOliVertexField],
  val vType: String,
  val size: Int = 0) {

  override def toString(): String = {
    "[" + "fields = " + fields + "," + "vType = " + vType + "," + "size = " + size + "]"
  }

  /**
   * Type quality
   * @other the other type
   * @return Boolean
   */
  override def equals(other: Any): Boolean = {
    return other match {
      case that: DsOliType =>
        DsOliLogger.debug("DsOliType::equals: called on element " + this + " with " + that)
        var index = 0
        var fieldMismatch = false
        if (that.fields == null || this.fields == null || that.fields.size != this.fields.size) {
          fieldMismatch = false
        } else {
          this.fields.foreach {
            field =>
              if (field != that.fields(index)) {
                DsOliLogger.debug("DsOliType::equals: found a mismatch:")
                DsOliLogger.debug("DsOliType::equals: \tfield=\t\t" + field)
                DsOliLogger.debug("DsOliType::equals: \tthat.fields("+index+")=\t" + that.fields(index))
                fieldMismatch = true
              }
              index+=1
          }
        }

        DsOliLogger.debug("DsOliType::equals: called on element " + this.vType + " == " + that.vType + " && " + this.size + " == " + that.size + " && " + fieldMismatch)
        this.vType == that.vType && this.size == that.size && !fieldMismatch
      case _ =>
        DsOliLogger.debug("DsOliCell::equals: called on wrong element ")
        false
    }
  }

}