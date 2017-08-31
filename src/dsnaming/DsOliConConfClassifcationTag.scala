
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
 * DsOliConConfClassifcationTag.scala created on Jul 18, 2015
 *
 * Description: Enumeration of the data structures detectable by DSI
 */
package dsnaming

/**
 * @author DSI
 *
 */
object DsOliConConfClassificationTag extends Enumeration {
  type DsOliConConfClassificationTag = Value
  // SHN: same head node
  // I2o: intersection on two nodes overlay
  // I1o: intersection on one node overlay
  // No: nesting on overlay
  // CDLL: cyclic doubly linked list
  // DLL: doubly linked list
  // ccNoClassification: no classification
  // ccNoPtrClassification: no pointer classification
  // I1o: intersection on one node indirect
  // Ni: nesting on indirection
  // SLo2: Skip list overlay
  // BT: binary tree
  val SHN, I2o, I1o, No, CDLL, DLL, ccNoClassification, ccNoPtrClassification, I1i, Ni, SLo2, BT = Value
}