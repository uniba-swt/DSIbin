
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
 * DsOliPathUtils.scala created on Mar 24, 2015
 *
 * Description: Bundle up helper functions for
 * handling the pathes used in the project
 */
package util

import java.io.File

/**
 * @author DSI
 *
 */
object DsOliPathUtils {

  // The project path leading to the trace.xml
  var project: String = ""
  // Any additional information append to the log dir
  var logDirAddition = ""
  // The name of the log dir
  var logDir = "log" + logDirAddition
  // The name of the log file
  var logFile = "log"
  // The name of dir for storing refined DSI types
  var refinedTypesDir = "dsi-refined-types"

  /**
   * Get the path to the trace.xml
   *
   * @return the path
   */
  def getXMLFile(): String = {
    project.reverse.replaceFirst("lmx.ecart/", "").replaceFirst("/.*", "").reverse
  }

  /**
   * Get the path to the DSI refined types dir
   *
   * @return path to the DSI refined types dir
   */
  def getRefinedTypesDir(): String = {
    val tmpPath = getPath() + refinedTypesDir + "/"
    val tmpFile = new File(tmpPath)
    if (!tmpFile.exists) tmpFile.mkdir()
    refinedTypesDir + "/"
  }

  /**
   * Get the name of the project by fetching the
   * folder name where the trace.xml is stored.
   *
   * @return the extracted name of the project
   */
  def getProjectName(): String = {
    val tmpFile = new File(project)
    val absolutePath = tmpFile.getAbsolutePath()
    val tmpPath = absolutePath.substring(0, absolutePath.lastIndexOf(File.separator))
    tmpPath.substring(tmpPath.lastIndexOf(File.separator) + 1)
  }

  /**
   * Get the project path, i.e., the path where the trace.xml lies
   *
   * @return the path to the trace.xml file
   */
  def getPath(): String = {
    val tmpFile = new File(project)
    val absolutePath = tmpFile.getAbsolutePath()
    val tmpPath = absolutePath.substring(0, absolutePath.lastIndexOf(File.separator)) + "/"
    val tmpDir = new File(tmpPath)
    if (!tmpDir.exists) tmpDir.mkdir()
    tmpPath
  }

  /**
   * Fetch the log dir of the project
   *
   * @return the log dir
   */
  def getLogDirPath(): String = {
    val tmpPath = getPath() + logDir + "/"
    val tmpFile = new File(tmpPath)
    if (!tmpFile.exists) tmpFile.mkdir()
    tmpPath
  }

  /**
   * Get the path of the log
   *
   * @return the log file path
   */
  def getLogFilePath(): String = {
    val tmpFilePath = getLogDirPath + logFile
    val tmpFile = new File(tmpFilePath)
    if (!tmpFile.exists) tmpFile.createNewFile()
    tmpFilePath
  }

}