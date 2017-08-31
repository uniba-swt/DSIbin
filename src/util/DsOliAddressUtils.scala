
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
 * DsOliAddressUtils.scala created on Mar 5, 2015
 *
 * Description: Utility class provides helper methods
 * for address calculations
 */
package util

import extlogger.DsOliLogger
import event.DsOliMemoryEvent
import PartialFunction._
import event.DsOliFreeEvent
import event.DsOliVLSEvent
import event.DsOliEvent
import pointstograph.DsOliVertexMemory
import pointstograph.DsOliVertex
import scalax.collection.mutable.Graph
import pointstograph.DsOliDiEdge

/**
 * @author DSI
 *
 */
object DsOliAddressUtils {
  val classSignature = "DsOliAddressUtils::"

  /**
   * Checks if address A is in range of address B
   *
   * @param vertexAStartAddress start address of vertex A
   * @param vertexAEndAddress end address of vertex A
   * @param vertexBStartAddress start address of vertex B
   * @param vertexBEndAddress end address of vertex B
   * @returns Boolean
   */
  def addressInRange(aStartAddress: Long, aEndAddress: Long, bStartAddress: Long, bEndAddress: Long): Boolean = {
    val funSignature = classSignature + "addressInRange: "
    // following condition is assumed: bStartAddress < bEndAddress!
    DsOliLogger.debug(funSignature + "entered: aStartAddress.toHexString = " +
      aStartAddress.toHexString + "; bStartAddress.toHexString = " + bStartAddress.toHexString)
    DsOliLogger.debug(funSignature + "\taEndAddress.toHexString = " +
      aEndAddress.toHexString + "; bEndAddress.toHexString = " + bEndAddress.toHexString)
    return aStartAddress <= bStartAddress && bEndAddress <= aEndAddress
  }

  /**
   * Fetch the start address from a free / VLS event
   *
   * @param event the free/VLS event
   * @return Option the address
   */
  def getStartAddressFromFreeVLS(event: DsOliEvent): Option[Long] = {
    condOpt(event) {
      case e: DsOliFreeEvent =>
        e.argValue
      case e: DsOliVLSEvent =>
        e.address
    }

  }

  /**
   * Checks, if we observed a register. See PIN tool
   *
   * @param startAddress the start address to test
   * @return Boolean
   */
  def isRegister(startAddress: Long): Boolean = {
    // Registers are modeled in the PIN tool, best case 
    // export them from PIN tool via XML
    addressInRange(0x08, 0x208, startAddress, startAddress)
  }

  /**
   * Checks, if vertex is on the stack, is a register or
   * on the heap.
   *
   * @param vertex the vertex to check
   * @param Boolean
   */
  def isStatic(vertex: DsOliVertexMemory): Boolean = {
    val funSignature = classSignature + "isStatic: "
    DsOliLogger.debug(funSignature + "entered: ")

    // Needs to be parsed from addresses file!
    val textStartAddress = 0x4008d0.toLong
    val bssEndAddress = 0x606000.toLong

    // Currently only a hard coded 
    val stackAddressBegin = "7f"
    val stackAddressBegin02 = "6"

    // Registers are also modeled now
    val isRegisterVertex = isRegister(vertex.bAddr)

    // We have a "static" vertex either if it is allocated on the stack OR
    // if the vertex is located in address range of .text.start ... .bss.end
    val isStackAllocated = vertex.bAddr.toHexString.startsWith(stackAddressBegin) || vertex.bAddr.toHexString.startsWith(stackAddressBegin02)
    val isGlobalOrStatic = (textStartAddress <= vertex.bAddr && vertex.bAddr <= bssEndAddress)

    DsOliLogger.debug(funSignature + "vertex : " + vertex)
    DsOliLogger.debug(funSignature + "\tisRegisterVertex: " + isRegisterVertex)
    DsOliLogger.debug(funSignature + "\tisStackAllocated: " + isStackAllocated)
    DsOliLogger.debug(funSignature + "\tisGlobalOrStatic: " + isGlobalOrStatic)

    isRegisterVertex || isStackAllocated || isGlobalOrStatic
  }

}