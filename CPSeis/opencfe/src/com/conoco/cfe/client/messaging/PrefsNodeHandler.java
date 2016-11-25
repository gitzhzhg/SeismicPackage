// PrefsNodeHandler.java

package com.conoco.cfe.client.messaging;

import org.w3c.dom.Node;

/**
 * Interface that can be implemented to create
 * a handler for a new node type in the 
 * preferences XML file.
 * 
 * @see preferences.dtd
 */
public interface PrefsNodeHandler {
  /**
   * This method is invoked by the parser when 
   * it encounters a document node.
   * 
   * @param n the document node
   */
  public void performAction(Node n);
}