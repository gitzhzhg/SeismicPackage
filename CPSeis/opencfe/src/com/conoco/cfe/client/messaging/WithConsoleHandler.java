// WithConsoleHandler.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import org.w3c.dom.Node;

/**
 * A handler for the console node. 
 */
public class WithConsoleHandler  implements PrefsNodeHandler {
  /**
   * This method is invoked when the parser
   * comes accross a "WithConsole" node.
   * 
   * @param n the document node
   */  
  public void performAction(Node n) {
    boolean value   = XMLHelper.getBooleanAttributeValue(n, "value");
    if (!value){
    Console.turnOffConsoleWindow();
    }
  }    
}