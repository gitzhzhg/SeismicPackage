// LoginServletNameHandler.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.gui.XMLHelper;

import org.w3c.dom.Node;

/**
 * A handler for the "LoginServletName" document 
 * node. 
 */
public class LoginServletNameHandler implements PrefsNodeHandler {
  /**
   * This method is invoked when the parser
   * comes accross a "LoginServletName" node.
   * 
   * @param n the document node
   */  
  public void performAction(Node n) {
    String nn = XMLHelper.getCharDataString(n);
    Preferences.setLoginServletName(nn);
  }    
}