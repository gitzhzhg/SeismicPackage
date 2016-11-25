// BackEndLibPathHandler.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.gui.XMLHelper;

import org.w3c.dom.Node;

/**
 * A handler for the "BackEndLibPath" document 
 * node. 
 */
public class BackEndLibPathHandler  implements PrefsNodeHandler {
  /**
   * This method is invoked when the parser
   * comes accross a "ServerName" node.
   * 
   * @param n the document node
   */  
  public void performAction(Node n) {
    String path = XMLHelper.getCharDataString(n);
    Preferences.setBackEndLibPath(path);
  }    
}