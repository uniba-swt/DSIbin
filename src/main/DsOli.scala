
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
 
 package main

import event._
import simpleopts.SimpleOpts
import scala.collection.mutable.ListBuffer
import org.xml.sax.SAXException
import pointstograph._
import pointstograph.DsOliDiEdge._
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scala.reflect.ClassTag
import scalax.collection.mutable.DefaultGraphImpl
import test._
import extlogger.DsOliLogger
import boxcalculation.DsOliBoxesCreator
import boxcalculation.IDsOliBoxCreator
import typeparser.TypeDB
import typeparser.DsOliTypeParser
import entrypoint.IDsOliEntryPointCreator
import entrypoint.DsOliEntryPointCreator
import entrypoint.IDsOliEPTWriter
import entrypoint.DsOliEPTXMLWriter
import util.DsOliPathUtils
import dsnaming.IDsOliDsNaming
import dsnaming.DsOliDsNaming
import output.DsOliCreateXML
import binarytypes.DsOliBinaryTypes
import binarytypes.DSITypesCreator

object DsOli {

  def main(args: Array[String]): Unit = {

    // ***** config area *********
    val debug = false
    val fileLog = false
    val version = "DsOli version 0.5.3"
    val naming = true
    val createXML = false
    val createEPTs = false
    val writeEPTs = false
    // ***** config area end *****

    DsOliLogger.configLogger(debug, fileLog)

    try {

      DsOliLogger.info(version)
      println(version)
      DsOliLogger.printLoggerStatus

      // Primitive parser for command line arguments
      val cmdLineParser = new SimpleOpts()
      cmdLineParser.parse(args)

      // Use a user specified log directory suffix
      if (cmdLineParser.parsedOptions.contains("logfolderaddition")) {
        println("Custom log directory suffix: " + cmdLineParser.parsedOptions("logfolderaddition"))
        DsOliPathUtils.logDir = DsOliPathUtils.logDir + cmdLineParser.parsedOptions("logfolderaddition")
      }

      // Save the current path to the trace file
      DsOliPathUtils.project = cmdLineParser.parsedOptions("xml");
      DsOliLogger.setLogPath(DsOliPathUtils.getLogFilePath)

      // Create the type DB
      val typeParser = new DsOliTypeParser(cmdLineParser.parsedOptions("typexml"))
      val typeDB: ITypeDB = typeParser.parseTypes()

      // Create the parser for the XML event trace
      val eventParser: IDsOliEventParser = new DsOliEventParser(
        cmdLineParser.parsedOptions("xml"),
        cmdLineParser.parsedOptions("xsd"),
        typeDB)

      // Parse the trace and return events container
      val events = eventParser.parse
      println("DsOli: events parsed")

      // Pass the created event trace and the type DB to the PTG creator
      val ptgCreator: IDsOliPTGCreator = new DsOliPTGCreator(events, typeDB)
      val (ptgs, mergedTypeGraph) = ptgCreator.createPTGs
      println("DsOli: ptg created")

      // Process the type refinement/merging to enrich the Howard types
      if (cmdLineParser.parsedOptions.contains("refine")) {
        val dsiTypesCreator = new DSITypesCreator(typeDB)
        val allTypeHypotheses = dsiTypesCreator.calculateDSITypes(mergedTypeGraph)
        println("Type refinements written. Exiting.")
        System.exit(1)
      }

      // Output all results thus far. Omit the boxes
      //DsOliTestMethods.printResults(events, ptgs, null, null, typeDB)

      // Box calculation
      val boxCreator: IDsOliBoxCreator = new DsOliBoxesCreator(events, ptgs, typeDB)
      val boxes = boxCreator.createBoxes
      println("DsOli: boxes created")

      // Output all results thus far. Omit the epts
      // DsOliTestMethods.printResults(events, ptgs, boxes, null, typeDB)

      // Entry point creator needs to be instantiated here to be able to pass it to naming step
      val epCreator: IDsOliEntryPointCreator = new DsOliEntryPointCreator(events, ptgs, boxes,
        boxCreator.asInstanceOf[DsOliBoxesCreator], typeDB)

      // Ds naming
      if (naming) {
        val dsNamer: DsOliDsNaming = new DsOliDsNaming(events, ptgs, boxes, boxCreator.asInstanceOf[DsOliBoxesCreator],
          epCreator.asInstanceOf[DsOliEntryPointCreator], typeDB)
        val (mbgs, mergedMbgs, aggGraph, epAggCnt, labeledAggGraph) = dsNamer.dsNaming
        println("DsOli: naming done")

        println("Total number of events: " + events.events.length)
        println("Memory events: " + events.events.count(event => event.isInstanceOf[DsOliMemoryEvent]))
        println("Memory write events: " + events.events.count(event => event.isInstanceOf[DsOliMWEvent]))

        epAggCnt.foreach {
          epTuple =>
            val (vep, vepStart, aggCnt) = epTuple
            println("vep.id: " + vep.id + ", vepStart: " + vepStart + ", aggCnt: " + aggCnt)
        }
      }

      // Create the XML output
      if (createXML) {
        val ptgToXML = new DsOliCreateXML(ptgs, boxes, typeDB, boxCreator.asInstanceOf[DsOliBoxesCreator])
        ptgToXML.createXML
        println("DsOli: xml created")
        DsOliTestMethods.printResults(events, ptgs, boxes, null, typeDB)
      }

      // Entry point calculation
      if (createEPTs) {
        val epts = epCreator.createEPTs()
        println("DsOli: epts created")
        if (writeEPTs) {
          // Write entry point trace to file
          val epWrite: IDsOliEPTWriter = new DsOliEPTXMLWriter(epts, events,
            cmdLineParser.parsedOptions("featuretracexsd"))
          epWrite.writeEPTTrace
          println("DsOli: epts written")
        }

      }

      // Finally print the graphs for each step
      //DsOliTestMethods.printResults(events, ptgs, boxes, epts, typeDB)

      // At the end flush the buffer, which might still hold some unwritten data
      DsOliLogger.flushBuffer

      println("DsOli: Done")
    } catch {
      case e: Throwable =>
        DsOliLogger.flushBuffer
        throw e
      case e: SAXException => DsOliLogger.error("XSD/XML Error: " + e.getMessage())
      case e: Exception => DsOliLogger.error("Error occured: " + e.getMessage())
    }
  }

}