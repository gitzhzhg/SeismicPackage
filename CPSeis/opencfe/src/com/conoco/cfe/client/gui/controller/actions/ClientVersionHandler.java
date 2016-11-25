// ClientVersionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;
import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.utils.ArrayList;

import com.conoco.xml.StringArray;

import org.w3c.dom.Node;
import org.w3c.dom.Document;
import org.w3c.dom.CharacterData;

/**
 * Action handler for the "ClientVersion" action.
 */
public class ClientVersionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {

  /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public ClientVersionHandler(AppGUIController.ActionHandlerHelper helper) {
    super(helper);  
  }
 
  /**
   * Performs the action as specified by the XML node describing the action.
   * This method is invoked by the message decoder. 
   * 
   * @param n the XML node for the message
   */  
  public void performAction(Node n) {
    String[] message = new String[1];
    message[0] = "Client Version number is: "+ClientConstants.getClientVersion();
    ErrorInfoWarningDialog.showInfoDialog(message);
  }        
}
