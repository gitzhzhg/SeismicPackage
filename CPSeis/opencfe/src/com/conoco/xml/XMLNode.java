// XMLNode.java

package com.conoco.xml;

import java.io.PrintWriter;

/**
 * Interface for a general XML node.
 */
public interface XMLNode {
  
  /**
   * Outputs the information in this node,
   * 
   * @param writer   the writer to which the information
   *           is to be sent to
   */
  void output(PrintWriter writer);
}