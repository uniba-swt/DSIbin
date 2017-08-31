
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
 * DsOliEPTXMLWriter.scala created on Feb 3, 2015
 *
 * Description: Write out the recored EPTs/features
 */
package entrypoint

import pointstograph.DsOliGraph
import pointstograph.DsOliVertex
import pointstograph.DsOliVertexMemory
import scala.collection.mutable.ListBuffer
import pointstograph.DsOliType
import scala.collection.mutable.HashMap
import java.io.PrintWriter
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Date
import entrypoint.Feature._
import event.DsOliEvents
import PartialFunction._
import event.DsOliMemoryEvent
import javax.xml.XMLConstants
import javax.xml.validation.SchemaFactory
import java.io.FileInputStream
import javax.xml.transform.stream.StreamSource
import extlogger.DsOliLogger
import xmltools.XMLXSDLoader
import java.io.File
import event.DsOliOTEvent
import event.DsOliOTEntryEvent
import event.DsOliOTEntryEvent
import event.DsOliOTExitEvent
import util.DsOliPathUtils
import event.DsOliEvent

/**
 * @author DSI
 *
 */
class DsOliEPTXMLWriter(featureSet: DsOliFeatureSet, events: DsOliEvents, xsdFilePath: String) extends IDsOliEPTWriter {

  val classSignature = "DsOliEPTXMLWriter::"
  val attributes = "#attributes#"
  val newLine = "\n"
  val traceTagName = "trace"
  val traceTagOpen = "<" + traceTagName + " " + attributes + ">" + newLine
  val traceTagClose = "</" + traceTagName + ">" + newLine
  val traceName = "name="
  val subTraceTagName = "subTrace"
  val subTraceTagNameOpen = "\t<" + subTraceTagName + " " + attributes + ">" + newLine
  val subTraceTagNameClose = "\t</" + subTraceTagName + ">" + newLine
  val subTraceId = "id="
  val subTraceAbsId = "absId="
  val subTraceStart = "eIdxStart="
  val subTraceEnd = "eIdxEnd="
  val subTraceFeatureStart = "fIdxStart="
  val subTraceFeatureEnd = "fIdxEnd="
  val subTraceFeature = "feature"
  val subTraceFeatureEIdx = "eIdx="
  val subTraceFeatureFIdx = "fIdx="
  val subTraceFeatureOpen = "\t\t<" + subTraceFeature + " " + attributes + ">"
  val subTraceFeatureClose = "</" + subTraceFeature + ">"

  // Lookup for absolute entry pointer IDs
  val eptAbsIdLookup = new HashMap[(String, Long, String, Long), Long]()

  /**
   * Create an absolute ID for an EPT by storing
   * all its features:
   * - type of the entry pointer vertex
   * - entry pointer offset
   * - cell type of most upstream cell
   * - linkage offset
   */
  def calculateAbsId(ept: DsOliEPT): Long = {
    val absId = (ept.epVertex.vType.vType, ept.oep, ept.Aup.cType.vType, ept.Aup.asInstanceOf[DsOliEPTCell].linkageOffset)
    val absIdValue = if (eptAbsIdLookup.contains(absId)) {
      // Fetch the stored ID
      eptAbsIdLookup.get(absId).get
    } else {
      // Create and insert the ID
      eptAbsIdLookup.put(absId, eptAbsIdLookup.size + 1)
      // size + 1 vs size => element was not inserted when the put
      // is executed
      eptAbsIdLookup.size
    }
    return absIdValue
  }

  /**
   * Get the current time step of all EPTs
   * represented as string.
   *
   * @param timeStep the curren time step
   */
  def getLine(timeStep: Long): (String, Int) = {
    var retLine = ""
    var lineCnt = 0
    featureSet.features.foreach {
      ept =>
        // Is the EPT alive?
        if (ept.creationTime <= timeStep && timeStep < ept.creationTime + ept.Qf.size) {
          // Fetch the symbol for the feature
          val curChar = Feature.featureMapping(ept.Qf((timeStep - ept.creationTime).toInt))
          retLine += curChar
          lineCnt += 1
        }
    }
    return (retLine, lineCnt)
  }

  /**
   * Get all EPTs which are alive in the
   * current time step
   *
   * @param timeStep the current time step
   * @return list of EPTs
   */
  def getAllFeaturesForTimeStep(timeStep: Int): ListBuffer[DsOliEPT] = {
    var retEPTs = new ListBuffer[DsOliEPT]()
    featureSet.features.foreach {
      ept =>
        if (ept.creationTime <= timeStep && timeStep < ept.creationTime + ept.Qf.size) {
          retEPTs.append(ept)
        }
    }
    retEPTs
  }

  /**
   * Extract the feature from the EPT for the given time step
   *
   * @param ept the entry pointer tag
   * @param timeStep the time step
   * @return the extracted feature
   */
  def getFeatureForTimeStepInEPT(ept: DsOliEPT, timeStep: Int): Feature = {
    ept.Qf((timeStep - ept.creationTime).toInt)
  }

  /**
   * Checks, that all EPTs only have no change features
   *
   * @param eptsForTimeStep the EPTs to check
   * @param timeStep the current time step
   * @return Boolean
   */
  def lineHasOnlyUnimportantEvents(eptsForTimeStep: ListBuffer[DsOliEPT], timeStep: Int): Boolean = {
    eptsForTimeStep.forall {
      ept =>
        val feature = getFeatureForTimeStepInEPT(ept, timeStep)
        !(feature == noChange || feature == plusOne || feature == plusN || feature == minusOne || feature == minusN || feature == newlyCreated ||
          feature == vlsNoChange)
    }

  }

  /**
   * Checks, if this is the last feature for the given EPT
   *
   * @param ept the entry pointer tag to check
   * @param timeStep the time step
   * @return Boolean
   */
  def isLastFeature(ept: DsOliEPT, timeStep: Int): Boolean = {
    ept.creationTime + ept.Qf.size - 1 == timeStep
  }

  /**
   * Write the given string to the file
   *
   * @param content the content to write
   * @param suffix the suffix for the file name
   */
  def writeFile(content: String, suffix: String): Unit = {
    val writer = new PrintWriter(new FileWriter(DsOliPathUtils.getPath() + DsOliPathUtils.getXMLFile() + suffix, false))
    writer.write(content + "\n")
    writer.close()
  }

  /**
   * Validate the written XML feature trace
   *
   * @param suffix the suffix for the file name to check
   */
  def validateFeatureTraceXML(suffix: String): Unit = {
    val factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    val xsdStream = new FileInputStream(new File(xsdFilePath))
    val schema = factory.newSchema(new StreamSource(xsdStream))
    DsOliLogger.debug("DsOliEPTXMLWriter::validateFeatureTraceXML: xsd done")

    val source = new FileInputStream(new File(DsOliPathUtils.getPath() + DsOliPathUtils.getXMLFile() + suffix))
    val xml = new XMLXSDLoader(schema).load(source)
    DsOliLogger.debug("DsOliEPTXMLWriter::validateFeatureTraceXML: xml done")
  }

  /**
   * Transform the given EPT/feature store into
   * a matrix representation, where each row
   * of the matrix corresponds to a time step
   * and each column corresponds to an EPT
   *
   * @param stepStore key: time step value: list of tuples (EPT id, feature)
   * @return the matrix as a string
   */
  def createMatrixOutput(stepStore: HashMap[Int, ListBuffer[(Long, Feature)]]): String = {
    // Do the calculation of the static part outside of the loop
    val featuresSorted = this.featureSet.features.sortBy(_.id)
    val featureLength = featureSet.features.length
    val curLine = new Array[Char](featureLength)

    // Create a StringBuilder with the expected capacity (featureLength == line length; + 2 for new line)
    var allLines = new StringBuilder((featureLength + 2) * this.events.events.length)

    // Cycle through all time steps and fetch the
    // EPTs and features for each time step
    for (i <- 0 until this.events.events.length) {

      print("Mtx out: " + i + "/" + this.events.events.size + "\r")

      var index = 0
      // Do the static things outside of the loop
      // Check, if the step store actually contains EPTs for
      // the current time step
      val stepStoreContainsI = stepStore.contains(i)

      // Cycle through all EPTs and fetch the 
      // features for the time step if they exist
      featuresSorted.foreach {
        ept =>

          // EPTs/features are present
          if (stepStoreContainsI) {

            // Get the features
            val featuresForStep = stepStore.get(i).get

            // Get the features for the EPT
            val featureAtTimeStepOpt = featuresForStep.find(_._1 == ept.id)

            // If features are present, write them out
            if (featureAtTimeStepOpt.isDefined) {
              curLine(index) = Feature.featureMapping(featureAtTimeStepOpt.get._2).charAt(0)
            } else {
              curLine(index) = ' '
            }
          } else {
            curLine(index) = ' '
          }
          index += 1
      }

      // Append the operation name, if present
      val opName =
        condOpt(this.events.events(i)) {
          case e: DsOliOTEntryEvent => e.name
          case e: DsOliOTExitEvent => "exit"
          case _ => ""
        }

      // Save everything
      allLines.append(opName.get)
      allLines.append(curLine.toSeq.mkString)
      allLines.append(newLine)

    }

    allLines.toString
  }

  /**
   * Create the output in a matrix style
   * and write it to file
   *
   * @param stepStore key: time step value: list of tuples (EPT id, feature)
   */
  def writeMatrix(stepStore: HashMap[Int, ListBuffer[(Long, Feature)]], suffix: String): Unit = {
    val allLines = createMatrixOutput(stepStore)
    writeFile(allLines, suffix)
  }

  /**
   * Remove duplicate features from the trace
   * for one entry pointer tag to avoid clutter.
   * Create an XML and a matrix output of the
   * results.
   *
   * @return string representation of the trace
   */
  def doDuplicateRemovalOnEvents(): String = {

    val funSignature = classSignature + "doDuplicateRemovalOnEvents: "

    var retString = new StringBuffer();
    retString.append(traceTagOpen.replace(attributes, traceName + "\"" + DsOliPathUtils.getXMLFile() + "\""))

    var eptId = 0
    var lastLine = ""

    // Key: TimeStep -> Value: ListBuffer[eptId, Feature]
    var stepStoreReduced = new HashMap[Int, ListBuffer[(Long, Feature)]]()

    var eptXMLTrace = new HashMap[Long, String]()

    var timeStep = 1
    var featureStep = 0
    var recordState = 0
    val spacing = "                "

    this.events.events.foreach {
      event =>

        // Get all EPTs for the current time step
        val eptsForTimeStep = getAllFeaturesForTimeStep(timeStep)

        print("Mtx: " + timeStep + "/" + this.events.events.size + "\r")

        // Fetch a string representation of the features
        val (lineUniform, lineCnt) = getLine(timeStep)

        // Check, if the line can be skipped due to unimportant
        // events only
        if (!lineHasOnlyUnimportantEvents(eptsForTimeStep, timeStep)) {

          // Important events are present

          // Now check, if the previous line is identical
          // to the current line. If identical -> skip the line
          if (lastLine != lineUniform) {
            // Lines are different, record
            featureStep += 1
            recordState = 1
            lastLine = lineUniform
          } else {
            // Skip this line
            recordState = 6
          }
        } else {
          // Skip this line
          recordState = 6
        }

        // Cycle through all EPTs of the current time step
        eptsForTimeStep.foreach {
          ept =>

            // Handle XML trace output
            if (!eptXMLTrace.contains(ept.id)) {
              var attributeString = new StringBuffer()
              attributeString.append(subTraceId)
              attributeString.append("\"")
              attributeString.append(ept.id)
              attributeString.append("\" ")
              attributeString.append(subTraceAbsId)
              attributeString.append("\"")
              attributeString.append(calculateAbsId(ept))
              attributeString.append("\" ")
              attributeString.append(subTraceStart)
              attributeString.append("\"")
              attributeString.append(ept.creationTime)
              attributeString.append("\" ")
              attributeString.append(subTraceEnd)
              attributeString.append("\"")
              attributeString.append(ept.creationTime + ept.Qf.size - 1)
              attributeString.append("\" ")
              attributeString.append(subTraceFeatureStart)
              attributeString.append("\"")
              attributeString.append(featureStep)
              attributeString.append("\" ")
              attributeString.append(subTraceFeatureEnd)
              attributeString.append("\"#featureEnd#\"")
              var eptString = subTraceTagNameOpen.replace(attributes, attributeString)
              eptXMLTrace.put(ept.id, eptString)
            }

            // Fetch the current feature
            val feature = getFeatureForTimeStepInEPT(ept, timeStep)

            //  -> store featureStep in subTraceFeatureEnd!
            if (isLastFeature(ept, timeStep)) {
              eptXMLTrace.put(ept.id, eptXMLTrace.get(ept.id).get.replace("#featureEnd#", featureStep.toString))
            }

            // Need a shift of minus one here, because EPT tags start from zero!
            var curTime = timeStep - 1

            // Setup storage for the current time step
            if (!stepStoreReduced.contains(curTime)) {
              stepStoreReduced.put(curTime, new ListBuffer[(Long, Feature)]())
            }

            // Matrix: Record everything including skipped lines
            //stepStore.get(curTime).get.append((ept.id, feature))
            // Matrix: Do not record skipped lines
            if (recordState == 1) {
              stepStoreReduced.get(curTime).get.append((ept.id, feature))
            }

            // XML
            if (recordState == 1) {
              val attrRpl = subTraceFeatureEIdx + "\"" + timeStep + "\" " + subTraceFeatureFIdx + "\"" + featureStep + "\""
              eptXMLTrace.put(ept.id, eptXMLTrace.get(ept.id).get + (subTraceFeatureOpen.replace(attributes, attrRpl)))
              eptXMLTrace.put(ept.id, eptXMLTrace.get(ept.id).get + Feature.featureMapping(feature))
              eptXMLTrace.put(ept.id, eptXMLTrace.get(ept.id).get + subTraceFeatureClose)
              eptXMLTrace.put(ept.id, eptXMLTrace.get(ept.id).get + "<!--feature: " + feature + "-->" + newLine)
            } else if (recordState == 6) {
              eptXMLTrace.put(ept.id, eptXMLTrace.get(ept.id).get + "\t\t<!--s  : " + subTraceFeatureEIdx + "\"" +
                timeStep + "\" " + subTraceFeatureFIdx + "\"" + featureStep +
                "\"" + spacing + "feature: " + feature + "-->" + newLine)
            }

            eptId += 1

        }
        timeStep += 1
    }

    // Build the XML string
    eptXMLTrace.toSeq.sortBy(_._1).foreach {
      tuple =>
        val eptString = eptXMLTrace.get(tuple._1).get
        retString.append(eptString)
        retString.append(subTraceTagNameClose)
    }
    retString.append(traceTagClose)

    // Write the XML and Matrix representation of the features
    val xmlFile = "-feature-trace-events-reduced.xml"
    try {
      // XML: write and validate 
      writeFile(retString.toString, xmlFile)
      validateFeatureTraceXML(xmlFile)

      // Matrix: write
      writeMatrix(stepStoreReduced, "-feature-trace-events-reduced.matrix")
    } catch {
      case e: Exception =>
        // Stop immediately
        throw e

      // Or write some information
      //writeFile(retString + e, xmlFile)
      //writeFile("<invalid>", xmlFile)
      //DsOliLogger.error(funSignature + "XML invalid: " + e)
      //DsOliLogger.error(funSignature + "File: " + DsOliPathUtils.getXMLFile + xmlFile + " not written.")
    }

    retString.toString
  }

  /**
   * Write the entry pointer tag trace
   */
  def writeEPTTrace(): String = {

    doDuplicateRemovalOnEvents()

  }
}