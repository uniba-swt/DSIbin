
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
 * DsOliFeatureSet.scala created on Jan 28, 2015
 *
 * Description: The feature set contains all
 * entry pointer tags (EPT) collected during program
 * execution and the associated features for
 * each EPT
 */
package entrypoint

import scala.collection.mutable.ListBuffer
import extlogger.DsOliLogger

/**
 * @author DSI
 *
 * @constructor create a feature set for holding all EPTs and its associated features
 * @param features list of EPTs
 */
class DsOliFeatureSet(var features: ListBuffer[DsOliEPT] = new ListBuffer[DsOliEPT]()) {
  val classSignature = "DsOliFeatureSet::"

  /**
   * Create a copy of the feature set
   * by creating deep copies of each
   * EPT in the set.
   *
   * @return a copy of the feature set
   */
  def deepCopy(): DsOliFeatureSet = {
    val funSignature = classSignature + "deepCopy:"
    DsOliLogger.debug(funSignature + "entered.")
    val newFeatures = this.features.map(f => f.deepCopy)
    return new DsOliFeatureSet(newFeatures)
  }

  override def toString(): String = {
    var retString = "[" + this.getClass() + ": features = ["
    features.foreach(
      feature =>
        retString += feature + ",")
    retString + "]]"
  }
}