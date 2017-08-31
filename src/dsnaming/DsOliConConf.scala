
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
 * DsOliConConf.scala created on Jun 19, 2015
 *
 * Description: A connection configuration representation
 */
package dsnaming

import dsnaming.DsOliConConfTag._

/**
 * @author DSI
 *
 * @constructor constructs a connection configuration
 * @param tag the type of the connection configuration
 * @param offsets the connection configuration offsets which quantify the connection between the strands
 */
class DsOliConConf(val tag: DsOliConConfTag, val offsets: (Long, Long)) {
  
  override def equals(that: Any): Boolean = {
    that match {
      case cc: DsOliConConf =>
        // Equality: tags need to be the same and the offsets need to match
        cc.tag == this.tag &&
          cc.offsets._1 == this.offsets._1 &&
          cc.offsets._2 == this.offsets._2
      case _ => false
    }
  }
  override def toString() : String = {
    "tag: " + this.tag + "; source offset: " + this.offsets._1 + "; target offset: " + this.offsets._2
  }
}