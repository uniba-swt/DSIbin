
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
 * SimpleOpts.scala created on Sep 24, 2014
 *
 * Description: Simple command line parser
 */
package simpleopts

/**
 * @author DSI
 *
 */
class SimpleOpts {
  var parsedOptions = collection.mutable.Map[String, String]()

  /**
   * Test if the command line parameter starts with the given
   * option. If it starts with the option, remove the option and
   * store the value of the option inside of parsedOptions.
   *
   * @param option the option string to test for
   * @param cmdLineOption the command line parameter to test
   */
  def testOption(option: String, cmdLineOption: String): Unit = {
    if (cmdLineOption.startsWith(option)) {
      parsedOptions.put(option.replaceAll("[^\\w]", ""), cmdLineOption.substring(option.length))
    }
  }

  /**
   * Parse the command line arguments and test for required parameters
   *
   * @param args the command line parameter
   */
  def parse(args: Array[String]): Unit = {
    if (args.length >= 4) {
      // Test mandatory parameters
      args.foreach {
        arg =>
          testOption("--xsd:", arg)
          testOption("--xml:", arg)
          testOption("--typexml:", arg)
          testOption("--featuretracexsd:", arg)
      }
      if (parsedOptions.size != 4) {
        throw new Exception("Invalid arguments")
      }
      // Optional parameters
      args.foreach{
        arg =>
          testOption("--refine", arg)
          testOption("--logfolderaddition:", arg)
      }
    } else {
      throw new Exception("Not enough arguments")
    }
  }
}