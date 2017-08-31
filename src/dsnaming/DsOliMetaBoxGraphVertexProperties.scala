
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
 * DsOliMetaBoxGraphVertexProperties.scala created on Jun 21, 2015
 *
 * Description: Helper class for storing and comparing
 * vertex properties of the various strand graphs
 */
package dsnaming

import pointstograph.DsOliType
import extlogger.DsOliLogger

/**
 * @author DSI
 *
 * @constructor creates an instance representing the properties of a vertex of the various strand graphs
 * @param isEP flag to indicate if the vertex holds an entry pointer
 * @param boxType the type of the strand(s) stored in the vertex
 * @param linkOffset the linkage offset of the strand(s) stored in the vertex
 */
class DsOliMetaBoxGraphVertexProperties(val isEP: Boolean, val boxType: DsOliType, val linkOffset: Long) {
  override def equals(that: Any): Boolean = {
    that match {
      case mbgProp: DsOliMetaBoxGraphVertexProperties =>
        DsOliLogger.debug("DsOliMetaBoxGraphVertexProperties::equals" +
          "(this.isEP && mbgProp.isEP): " + this.isEP + " && " + mbgProp.isEP +
          "(this.boxType == mbgProp.boxType && this.linkOffset == mbgProp.linkOffset): " +
          this.boxType + " == " + mbgProp.boxType + " && " + this.linkOffset +
          " == " + mbgProp.linkOffset)
        // Important: The current setup allows that static entry pointers in the case of the linux DLL example
        // can be actually displayed. Especially the line this.isEP != mbgProp.isEP
        if (this.isEP && mbgProp.isEP) {
          true
        } else if (this.isEP != mbgProp.isEP) {
          false // If they are not equal, just report false
        } else if ((this.boxType == mbgProp.boxType && this.linkOffset == mbgProp.linkOffset)) {
          true
        } else {
          false // Default
        }
      case _ => false
    }
  }
}