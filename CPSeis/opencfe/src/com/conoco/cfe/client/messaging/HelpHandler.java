// HelpHandler.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.gui.XMLHelper;

import org.w3c.dom.Node;

/**
 * A handler for the "Help" document 
 * node. 
 */
public class HelpHandler implements PrefsNodeHandler {
  /**
   * This method is invoked when the parser
   * comes accross a "HelpMode" node.
   * 
   * @param n the document node
   */  
  public void performAction(Node n) {
    String help = XMLHelper.getStringAttributeValue(n, "value");
    Preferences.setHelpMode(help);
  }    
}