// ScriptNameHandler.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.gui.XMLHelper;

import org.w3c.dom.Node;

/**
 * A handler for the "ScriptName" document 
 * node. 
 */
public class ScriptNameHandler implements PrefsNodeHandler {
  /**
   * This method is invoked when the parser
   * comes accross a "ScriptName" node.
   * 
   * @param n the document node
   */  
  public void performAction(Node n) {
    String scriptName = XMLHelper.getCharDataString(n);
    Preferences.setScriptName(scriptName);
  }    
}