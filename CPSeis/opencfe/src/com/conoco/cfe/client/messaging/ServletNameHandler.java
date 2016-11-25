// ServletNameHandler.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.gui.XMLHelper;

import org.w3c.dom.Node;

/**
 * A handler for the "ServletName" document 
 * node. 
 */
public class ServletNameHandler  implements PrefsNodeHandler {
  /**
   * This method is invoked when the parser
   * comes accross a "ServletName" node.
   * 
   * @param n the document node
   */  
  public void performAction(Node n) {
    String serName = XMLHelper.getCharDataString(n);
    Preferences.setServletName(serName);
  }    
}