// ErrorActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.gui.XMLHelper;
import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;

import com.conoco.cfe.client.gui.controls.WindowControl;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.utils.ArrayList;

import com.conoco.xml.StringArray;

import org.w3c.dom.Node;
import org.w3c.dom.Document;
import org.w3c.dom.CharacterData;

import javax.swing.JFrame;

/**
 * Action handler for the "Error" action.
 */
public class ErrorActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {

  /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public ErrorActionHandler(AppGUIController.ActionHandlerHelper helper) {
    super(helper);  
  }
 
  /**
   * Performs the action as specified by the XML node describing the action.
   * This method is invoked by the message decoder. 
   * 
   * @param n the XML node for the message
   */  
  public void performAction(Node n) {

//SMCook window logic to get modal dialogs working properly (not accidentally hidden)
    int windowId = XMLHelper.getIntAttributeValue(n, "windowId");
    WindowControl wc = AppGUIController.getTopLevelWindow(windowId);

    String message = XMLHelper.getStringAttributeValue(n, "elements");
    String [] messages = StringArray.parseStringArray(message);
  
    JFrame f = wc.getJFrame();
    if(f == null)
      ErrorInfoWarningDialog.showErrorDialog(messages);
    else
      ErrorInfoWarningDialog.showErrorDialog(messages,f);
}        
}
