
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
 * DsOliEPT.scala created on Jan 28, 2015
 *
 * Description: Represents an entry pointer tag
 * used for operation detection
 */
package entrypoint

import scala.collection.mutable.ListBuffer
import pointstograph.DsOliVertexMemory
import entrypoint.Feature._
import extlogger.DsOliLogger
import scala.collection.mutable.HashMap

/**
 * @author DSI
 *
 * @constructor creates an entry pointer tag
 * @param epVertex the entry pointer vertex
 * @param epVertexId the id of the entry pointer vertex
 * @param oep the offset where the pointer originates at ep vertex
 * @param Aup most upstream cell
 * @param creationTime time step of the event trace when this EPT was created
 * @param Qf list of observed features for this EPT
 */
class DsOliEPT(val id: Long,
  var epVertex: DsOliVertexMemory,
  var epVertexId: Long,
  var oep: Long,
  var Aup: DsOliEPTCell,
  var creationTime: Long,
  var Qf: ListBuffer[Feature]) {

  // Store possible artificial features for a particular time step
  var artificialQfs = new HashMap[Int, ListBuffer[Feature]]()

  val classSignature = "DsOliEPT::"

  def this(epVertex: DsOliVertexMemory, epVertexId: Long, oep: Long, Aup: DsOliEPTCell, creationTime: Long, Qf: ListBuffer[Feature]) =
    this(DsOliEPT.getId, epVertex, epVertexId, oep, Aup, creationTime, Qf)

  /**
   *  Deep copy of an EPT, where each feature
   *  of the feature list is copied over
   *
   *  @return a copy of the EPT
   */
  def deepCopy(): DsOliEPT = {
    val funSignature = classSignature + "deepCopy: "
    DsOliLogger.debug(funSignature + "entered.")
    val newQf = this.Qf.map(f => identity(f))
    DsOliLogger.debug(funSignature + "newQf: " + newQf)
    return new DsOliEPT(this.id, this.epVertex, this.epVertexId, this.oep, this.Aup, this.creationTime, newQf)
  }

  override def toString(): String = {
    "[" + this.getClass() + ": epVertexId = " + epVertexId + "," + "oep = " + oep.toHexString +
      "," + "Aup = " + Aup + "," + "creationTime = " + creationTime.toHexString + "," + "Qf = " + Qf + "]"

  }
}

object DsOliEPT {
  type EPTId = Long
  var id: Long = 0

  /**
   * Simple unique EPT ID generator
   * @return unique EPT ID
   */
  def getId(): Long = {
    val retId = id;
    id += 1
    return retId
  }
}
