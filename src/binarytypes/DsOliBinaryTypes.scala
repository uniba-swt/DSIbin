
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
 * DsOliBinaryTypes.scala created on May 12, 2016
 *
 * Description: Helper for creating memory subregions from pointer mappings
 */
package binarytypes

import scala.collection.mutable.ListBuffer
import pointstograph.DsOliVertexMemory
import pointstograph.DsOliVertexMemory

/**
 * @author DSI
 *
 */
class DsOliBinaryTypes {
 
  def createSubRegionsForMemChunk(vertex: DsOliVertexMemory): ListBuffer[DsOliVertexMemory] = {
    val subRegions = new ListBuffer[DsOliVertexMemory]()
    val vertexSize = vertex.eAddr - vertex.bAddr

    // Only size two or more
    if (vertexSize < 2) return subRegions

    // Subregions have size between 2 <= subSize <= size of vertex - 1
    val startSize = 2L
    val endSize = vertexSize - 1
    var id = 0
    println("startSize: " + startSize + " endSize: " + endSize)
    
    // Iterate over all possible subregion sizes
    for (subSize <- startSize to endSize) {
      
      // Calculate range of offsets
      val offsetEnd = vertexSize - subSize
      println("subSize: " + subSize + " offsetEnd: " + offsetEnd)
      
      // Iterate over all offsets
      for (offset <- 0L to offsetEnd) {
        println("\toffset: " + offset)
        val subRegion = new DsOliVertexMemory(vertex.bAddr + offset, vertex.bAddr + offset + subSize, null, id)
        id += 1
        subRegions.append(subRegion)
      }
    }

    subRegions.foreach {
      subRegion =>
        println("subRegion: " + subRegion.id + " offset: " + (subRegion.bAddr - vertex.bAddr) + " size: " + (subRegion.eAddr - subRegion.bAddr) + " bAddr: " + subRegion.bAddr + " eAddr: " + subRegion.eAddr)
    }

    subRegions
  }
  
  /*
  def createSubRegionCombinations(subRegions: ListBuffer[DsOliVertexMemory]) : Unit = {
    val results = new ListBuffer[ListBuffer[DsOliVertexMemory]]()
    subRegions.foreach{
      subRegion =>
        val tmpResult = new ListBuffer[DsOliVertexMemory]()
        val subRegions = 
        createSubRegionCombinationsRec(tmpResult, results, subRegions)
        
    }
  }

  def createSubRegionCombinationsRec(tmpResult : ListBuffer[DsOliVertexMemory],  
      results : ListBuffer[ListBuffer[DsOliVertexMemory]],
      subRegions: ListBuffer[DsOliVertexMemory]) : Unit = {

  }*/

}