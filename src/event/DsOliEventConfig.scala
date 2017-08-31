
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
 * DsOliEventConfig.scala created on Sep 23, 2014
 *
 * Description: Keeps the names for the XML 
 * tags and attributes found in the event trace
 */
package event

/**
 * @author DSI
 *
 */
object DsOliEventConfig {
  val Id = "id"
  val Event = "event"
  val Entry = "entry"
  val Exit = "exit"
  val Name = "name"
  val Argument = "arguments"
  val Comment = "comment"
  val OperationTransition = "operation-transition"
  val BlockTransition = "block-transition"
  val MemoryWrite = "memory-write"
  val VariableEnterScope = "variable-enter-scope"
  val VariableLeftScope = "variable-left-scope"
  val Free = "free"
  val SourceLocation = "sourceLocation"
  val Kind = "kind"
  val SourceFile = "file"
  val SourceLine = "line"
  val SourceLineColumn = "column"
  val Lvalue = "lval"
  val Address = "address"
  val Type = "type"
  val CodeFragment = "codeFragment"
  val Content = "content"
  val MemoryAllocation = "memory-allocation"
  val Malloc = "malloc"
  val ArgCodeFragment = "argCodeFragment"
  val ArgValue = "argValue"
  val Calloc = "calloc"
  val ArgChunkSize = "argChunkSize"
  val ArgTimes = "argTimes"
  val Nil = "(nil)"
  val Context = "context"
  val LvalDerefType = "lvalDerefType"
  val RhsCodeFragment = "rhsCodeFragment"
}