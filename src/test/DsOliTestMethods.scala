
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
 * DsOliTestMethods.scala created on Oct 15, 2014
 *
 * Description: Mainly for printing results
 */
package test

import event._
import pointstograph._
import simpleopts._
import xmltools._
import simpleopts.SimpleOpts
import scala.collection.mutable.ListBuffer
import org.xml.sax.SAXException
import pointstograph.DsOliDiEdge._
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scala.reflect.ClassTag
import scalax.collection.mutable.DefaultGraphImpl
import extlogger._
import scalax.collection.io.dot._
import scalax.collection.edge.LDiEdge
import java.io._
import java.util.Locale
import java.util.Date
import java.text.SimpleDateFormat
import output.IDsOliGraphOutput
import output.DsOliDotGraphOutput
import boxcalculation.DsOliBoxSteps
import boxcalculation.DsOliBoxStep
import boxcalculation.DsOliBox
import boxcalculation.DsOliCell
import boxcalculation.DsOliCycle
import entrypoint.DsOliFeatureSet
import PartialFunction._
import event.DsOliMemoryEvent
import util.DsOliPathUtils
import dsnaming.DsOliMetaBoxGraph
import dsnaming.DsOliDotMbgOutput
import util.DsOliTimeStepContainer
import dsnaming.DsOliGraphCleaner
import typeparser.TypeDB

/**
 * @author DSI
 *
 */
object DsOliTestMethods {

  val classSignature = "DsOliTestMethods::"
  var firstInvocation = true
  var typeDB: ITypeDB = null

  /**
   * Print all given PTGs
   *
   * @param ptg the set of points to graphs to print
   * @param typeDB the type storage
   * @param boxSteps the all strand sets for the event trace
   * @param epts all entry pointer tags
   */
  def printPTGs(ptg: DsOliPointsToGraphs, typeDB: ITypeDB, boxSteps: Option[DsOliBoxSteps] = None, epts: DsOliFeatureSet): Unit = {
    var i = 0
    val creationDate = new Date()
    ptg.graphs.foreach {
      dsOliGraph =>
        val boxesForStep = if (boxSteps.isDefined) Some(boxSteps.get.boxSteps(i)) else None
        printPTG(dsOliGraph, typeDB, boxesForStep, null, i, 0, epts, "", false)
        i += 1
    }
  }

  /**
   * Print all given events
   *
   * @param events the event trace
   * @param ptgs all PTGs corresponding to the event trace
   * @param boxes all strand sets corresponding to the event trace
   * @param epts the entry pointers used for operation detection
   * @param typeDB the type storage
   */
  def printResults(events: DsOliEvents, ptgs: DsOliPointsToGraphs, boxes: DsOliBoxSteps, epts: DsOliFeatureSet, typeDB: ITypeDB): Unit = {
    val funSignature = classSignature + "printResults:"
    var i = 1

    // Cycle through all events
    events.events.foreach {
      event =>
        DsOliLogger.debug(funSignature + "#Event " + i + " #")
        condOpt(event) {
          case event: DsOliMemoryEvent =>

            // Boxes can be omitted
            if (boxes == null) {
              DsOliTestMethods.printPTG(ptgs.get(i), typeDB, None, null, i, 0, epts, "_c_event_id_" + event.id, false)
            } else {
              // First do the artificial dot files
              if (event.artificialEvents.length != 0) {
                val curBox = boxes.get(i).get
                for (u <- 0 until event.artificialEvents.length) {
                  val artBox = curBox.artificialBoxes(u)
                  val artPtg = ptgs.get(i).artificialGraphs(u)
                  DsOliTestMethods.printPTG(artPtg, typeDB, Some(artBox), null, i, u, epts, "_c_event_id_" + event.id + "_art_" + u, true)
                }
              }
              // Finally display the top level
              DsOliLogger.debug(funSignature + "passing to printPTG box: " + boxes.get(i))
              DsOliTestMethods.printPTG(ptgs.get(i), typeDB, boxes.get(i), null, i, 0, epts, "_c_event_id_" + event.id, false)

            }
        }
        DsOliLogger.debug(funSignature + "#Done Event " + i + " #")
        i += 1
    }

  }

  /**
   * Print a PTG
   *
   * @param ptg the points-to graph
   * @param typeDB the type storage
   * @param boxesForStep Option the set of strands for the time step (can be omitted)
   * @param date a data stamp
   * @param artIndex the index of an artificial event
   * @param epts the entry pointer tag feature set
   * @param nameAddtion can be used to annotate the written file names
   * @param isArtificial indicate, if it is an artificial event
   * @param cleanup flag to indicate if previously created graphs should be deleted
   */
  def printPTG(ptg: DsOliGraph, typeDB: ITypeDB, boxesForStep: Option[DsOliBoxStep],
    date: Date = null, i: Int, artIndex: Int, epts: DsOliFeatureSet,
    nameAddition: String = "", isArtificial: Boolean, cleanup: Boolean = true): Unit = {

    val dirPath = DsOliPathUtils.getLogDirPath
    // Delete all previous graph files
    if (firstInvocation && cleanup) {
      val dir = new File(dirPath)
      if (dir.exists()) {
        dir.listFiles().foreach {
          file =>
            if (file.getAbsolutePath().contains("graph_")) {
              file.delete()
            }
        }

      } else {
        dir.mkdir()
      }
      firstInvocation = false
    }

    // DOT output
    val graphOutputter: IDsOliGraphOutput = new DsOliDotGraphOutput(typeDB)
    val dotString = graphOutputter.createGraphOutput(ptg, boxesForStep, epts, i, artIndex, isArtificial)
    val dateString = if (date != null) {
      new SimpleDateFormat("ddMYYYYHms").format(date) + "_"
    } else {
      ""
    }
    val writer = new PrintWriter(new File(String.format(dirPath + "graph_" + dateString + "%08d" + nameAddition + ".dot", new Integer(i))))
    writer.write(dotString)
    writer.close()

  }

  /**
   * Clean all files in the given directory
   *
   * @param dir the directory which gets cleaned
   */
  def cleanDirectory(dir: String): Unit = {
    val dirObj = new File(dir)
    if (dirObj.exists) dirObj.listFiles().foreach(_.delete)
  }

  /**
   * Create a directory
   *
   * @param dirPath path to create
   * @return Boolean
   */
  def createDir(dirPath: String): Boolean = {
    val dir = new File(dirPath)
    if (!dir.exists()) dir.mkdirs() else true
  }

  /**
   * Print the SGs (strand graph; previously meta box graph), FSGs (folded strand graph;
   * previously merged meta box graph) and ASGs (aggregated strand graph).
   *
   * @param mbgs the strand graphs
   * @param mergedMBgs the folded strand graphs
   * @param aggGraph the aggregated strand graphs
   * @param labeled optional label for the log dir path
   * @param boxSteps all strand sets for the event trace
   */
  def printMbgs(mbgs: DsOliTimeStepContainer[DsOliMetaBoxGraph], mergedMbgs: DsOliTimeStepContainer[DsOliMetaBoxGraph],
    aggGraph: DsOliTimeStepContainer[(DsOliVertexMemory, DsOliMetaBoxGraph)], labeled: String = "", boxSteps: DsOliBoxSteps): Unit = {
    // Print the meta box graph
    var dirPath = DsOliPathUtils.getLogDirPath + "/mbg/"
    if (mbgs != null) {
      createDir(dirPath)
      cleanDirectory(dirPath)
      for (i <- 0 until mbgs.steps.length) {
        val mbg = mbgs.steps(i)
        // DOT output
        val graphOutputter = new DsOliDotMbgOutput()
        val dotString = graphOutputter.createGraphOutput(mbg, boxSteps)
        val dateString = new SimpleDateFormat("ddMYYYYHms").format(new Date) + "_" + (new Date).getTime()
        val nameAddition = "_mbg_event_id_" + (i + 1)
        val writer = new PrintWriter(new File(String.format(dirPath + "graph_" + dateString + "%08d" + nameAddition + ".dot", new Integer(i + 1))))
        writer.write(dotString)
        writer.close()
      }
    }

    // Print the merged meta box graph
    if (mergedMbgs != null) {
      dirPath = DsOliPathUtils.getLogDirPath + "/mmbg/"
      createDir(dirPath)
      cleanDirectory(dirPath)
      for (i <- 0 until mergedMbgs.steps.length) {
        val mbg = mergedMbgs.steps(i)
        // DOT output
        val graphOutputter = new DsOliDotMbgOutput()
        val dotString = graphOutputter.createGraphOutput(mbg, boxSteps)
        val dateString = new SimpleDateFormat("ddMYYYYHms").format(new Date) + "_"
        val nameAddition = "_mmbg_event_id_" + (i + 1)
        val writer = new PrintWriter(new File(String.format(dirPath + "graph_" + dateString + "%08d" + nameAddition + ".dot", new Integer(i + 1))))
        writer.write(dotString)
        writer.close()
      }
    }

    if (aggGraph != null) {
      // Print the agg graph
      dirPath = DsOliPathUtils.getLogDirPath + labeled + "/agg/"
      createDir(dirPath)
      cleanDirectory(dirPath)
      for (i <- 0 until aggGraph.steps.length) {
        val (ep, mbg) = aggGraph.steps(i)
        // DOT output
        val cleaned = DsOliGraphCleaner.cleanGraph(mbg)
        val graphOutputter = new DsOliDotMbgOutput()
        val dotString = graphOutputter.createGraphOutput(mbg, boxSteps)
        val cleanedDotString = graphOutputter.createGraphOutputUnDi(cleaned, this.typeDB, boxSteps)
        val dateString = new SimpleDateFormat("ddMYYYYHms").format(new Date) + "_"
        val nameAddition = "_agg_event_id_" + (i + 1) + "_ep_vertex_" + ep.id
        var writer = new PrintWriter(new File(String.format(dirPath + "graph_" + dateString + "%08d" + nameAddition + ".dot", new Integer(i + 1))))
        writer.write(dotString)
        writer.close()
        writer = new PrintWriter(new File(String.format(dirPath + "graph_cleaned_" + dateString + "%08d" + nameAddition + ".dot", new Integer(i + 1))))
        writer.write(cleanedDotString)
        writer.close()

      }
    }
  }

  /**
   * Generate a file name
   *
   * @param ep used for adding the ep id
   * @param t the time step
   * @param nameAdditionExt addition added to the name
   * @return the generate file name
   */
  def genFileName(ep: DsOliVertexMemory, t: Int, nameAdditionExt: String = ""): String = {
    val dateString = ""
    val nameAddition = "_" + nameAdditionExt + "_agg_event_id_" + 1 + "_ep_vertex_" + ep.id
    String.format("graph_" + dateString + "%08d" + nameAddition + ".dot", new Integer(t))
  }
}