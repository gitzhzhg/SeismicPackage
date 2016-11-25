// ModifyProcessActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.utils.ArrayList;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "ModifyProcess" action.
 */
public class ModifyProcessActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
    /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public ModifyProcessActionHandler(AppGUIController.ActionHandlerHelper helper) {
    super(helper);  
  }

  /**
   * Performs the action as specified by the XML node describing the action.
   * This method is invoked by the message decoder. 
   * 
   * @param n the XML node for the message
   */  
  public void performAction(Node n) {
    int pid = XMLHelper.getIntAttributeValue(n, "windowId");
    
    // PENDING ( mangesh ) 9/28/99
    System.err.println("ModifyProcessActionHandler");
  }        
}