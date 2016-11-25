// BeepActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import java.awt.Toolkit;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "Beep" action.
 */
public class BeepActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
    /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public BeepActionHandler(AppGUIController.ActionHandlerHelper helper) {
    super(helper);  
  }

  /**
   * Performs the action as specified by the XML node describing the action.
   * This method is invoked by the message decoder. 
   * 
   * @param n the XML node for the message 
   */  
  public void performAction(Node n) {
    Toolkit.getDefaultToolkit().beep();    
  }        
}