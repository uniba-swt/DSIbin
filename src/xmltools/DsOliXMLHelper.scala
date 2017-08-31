
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
 * DsOliXMLHelper.scala created on Nov 4, 2014
 *
 * Description: Helper to parse XML
 */
package xmltools

import scala.xml.Node
import extlogger.DsOliLogger

/**
 * @author DSI
 *
 */
object DsOliXMLHelper {

  /**
   * Fetch the tag node
   *
   * @param node the XML node
   * @param tag the name of the tag
   * @return Option node representing the tag
   */
  def getTagNode(node: Node, tag: String): Option[Node] = {
    val tagSeq = getTagSeq(node, tag)
    val tagNode =
      if (tagSeq.length != 0) {
        Some(tagSeq.head)
      } else {
        None
      }
    return tagNode
  }

  /**
   * Get the sequence of nodes for a tag
   *
   * @param node the XML node
   * @param tag the tag name
   * @return Sequence of XML nodes for the tag
   */
  def getTagSeq(node: Node, tag: String): Seq[Node] = {
    return (node \ tag)
  }

  /**
   * Fetch the tag value
   *
   * @param node the XML node
   * @param attrib the name of the tag
   * @return return the string value of the tag
   */
  def getTagValue(node: Node, tag: String): String = {
    val tagNodeOpt = getTagNode(node, tag)
    val tagNodeText =
      if (tagNodeOpt.isDefined) {
        tagNodeOpt.get.text
      } else {
        ""
      }
    return tagNodeText
  }

  /**
   * Fetch the attribute value
   *
   * @param node the XML node
   * @param attrib the name of the attribute
   * @return return the string value of the attribute
   */
  def getTagAttribute(node: Node, attrib: String): String = {
    return (node \ ("@" + attrib)).text
  }

  /**
   * Convert a given string to an int by using the base for the conversion
   *
   * @param longString the value string
   * @param base the base used for the conversion
   * @return the converted value
   */
  def parseInt(intString: String, base: Int): Int = {
    val retVal: Int =
      try {
        Integer.parseInt(intString, base)
      } catch {
        case e: Throwable => 0
      }
    return retVal
  }

  /**
   * Convert a given string to a long by using the base for the conversion
   *
   * @param longString the value string
   * @param base the base used for the conversion
   * @return the converted value
   */
  def parseLong(longString: String, base: Int): Long = {
    val retVal: Long =
      try {
        java.lang.Long.parseLong(longString, base)
      } catch {
        case e: Throwable =>
          DsOliLogger.error("DsOliXMLHelper::parseLong: " + longString + ":: " + e)
          0
      }
    return retVal
  }

  /**
   * Convert string value to string "0" in case of empty string
   *
   * @param intVal the value as a string
   * @returns the value string or "0" in case of empty string
   */
  def valueOrZero(intVal: String): String = {
    return if (intVal == "") "0" else intVal
  }
}