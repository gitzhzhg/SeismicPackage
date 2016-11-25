// XmlActionhandler.java

package com.conoco.cfe.client.gui.controller.actions;

import org.w3c.dom.Node;

/**
 * interface implemented by every handler
 */
public interface XmlActionHandler {
  
  /**
   * This method is invoked by the XML decoder 
   * when it parses an XML message node.
   * 
   * @param node the XML message node
   */
  public void performAction(Node node);
}