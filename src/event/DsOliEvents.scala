
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
 * DsOliEvents.scala created on Oct 1, 2014
 *
 * Description: Represents the event trace
 */
package event

import scala.reflect.ClassTag

/**
 * @author DSI
 *
 */
class DsOliEvents(val events: Array[DsOliEvent]) {
  
  /**
   * Important: Initialization of the reference for all
   * event objects to refer to this instance of the
   * container.
   */
  DsOliEvent.events = this
  
  /**
   * Get the event of type T either by searching backwards or forward
   * in the array. The search direction is given by the incDec method
   * passed to the function.
   * 
   * @param incDec method produces next index
   * @param id event identifier
   */
  def findEventOfType[T : ClassTag](incDec : Int => Int, id : Int) : Option[T] = {
    var result : Option[T] = None
    // Skip current element
    var i = incDec(id)
    
    while(i < events.length && i >= 0) {
      events(i) match {
        case e: T =>
          result = Some(events(i).asInstanceOf[T])
          i = events.length
        case _ =>
          i = incDec(i)
      }
    }
    
    return result
  }

  /**
   * Find the next event of type T starting out
   * at the given ID
   * 
   * @param id the index to start
   * @return Option the found element
   */
  def getNextEventOfType[T: ClassTag](id: Int): Option[T] = {
    return findEventOfType[T]((_+1),id)
  }
  
  /**
   * Find the previous event of type T starting out
   * at the given ID
   * 
   * @param id the index to start
   * @return Option the found element
   */
  def getPrevEventOfType[T: ClassTag](id: Int): Option[T] = {
    return findEventOfType[T]((_-1),id)
  }
  
}
