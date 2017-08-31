
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
 * DsOliEvent.scala created on Sep 22, 2014
 *
 * Description: Represents the base class for all events
 */
package event

import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer

/**
 * @author DSI
 * @constructor create an event with a unique id and a source location
 * @param id unique id for an event
 * @param sourceLocation the location of the event in the source code
 */
class DsOliEvent(val id : Int, val sourceLocation : DsOliSourceLocation) {

  // Storage for possible artificial events
  var artificialEvents = new ListBuffer[DsOliEvent]()

  def getNextEventOfType[T: ClassTag](): Option[T] = {
    return DsOliEvent.events.getNextEventOfType[T](id)
  }
  
  def getPrevEventOfType[T: ClassTag](): Option[T] = {
    return DsOliEvent.events.getPrevEventOfType[T](id)
  }
}
object DsOliEvent {
  var events : DsOliEvents = null
}