
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
 * DsOliFeature.scala created on Mar 16, 2015
 *
 * Description: The feature recorded for a strand.
 * Features describe changes in a strand, such
 * as plus or minus one element from one time
 * step to the other.
 *
 */
package entrypoint

/**
 * @author DSI
 *
 */
object Feature extends Enumeration {

  type Feature = Value

  val newlyCreated, noChange, plusOne, minusOne, plusN, minusN, btEntryNoChange, btExitNoChange, freeNoChange, commentNoChange, otEntryNoChange, otExitNoChange, vlsNoChange, freeOrVLS = Value

  val classSignature = "Feature::"

  /**
   * Mapping from a Feature to a string representation
   *
   * @return the string representation
   */
  def featureMapping(feature: Feature): String = {
    val funSignature = classSignature + "featureMapping: "
    feature match {
      case Feature.newlyCreated => "N"
      case Feature.noChange => "x"
      case Feature.plusOne => "+"
      case Feature.plusN => "P"
      case Feature.minusOne => "-"
      case Feature.minusN => "m"
      case Feature.btEntryNoChange => "b"
      case Feature.btExitNoChange => "e"
      case Feature.freeNoChange => "f"
      case Feature.commentNoChange => "c"
      case Feature.otEntryNoChange => "o"
      case Feature.otExitNoChange => "p"
      case Feature.vlsNoChange => "v"
      case Feature.freeOrVLS => "F"
      case _ => throw new Exception(funSignature + "Unkown feature: " + feature)
    }
  }
}
