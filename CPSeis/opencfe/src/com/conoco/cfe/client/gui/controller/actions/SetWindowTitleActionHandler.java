// SetWindowTitleActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.WindowControl;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "SetWindowTitle" action.
 */
public class SetWindowTitleActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
   /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public SetWindowTitleActionHandler(AppGUIController.ActionHandlerHelper helper) {
    super(helper);  
  }

  /**
   * Performs the action as specified by the XML node describing the action.
   * This method is invoked by the message decoder.
   * 
   * @param n the XML node for the message
   */  
  public void performAction(Node n) {
    String title = XMLHelper.getStringAttributeValue(n, "value");    
    int pid = XMLHelper.getIntAttributeValue(n, "windowId");
  
    WindowControl control = getHelper().getTopLevelWindow(pid);

    if (control != null) {
      control.setTitle(title);
    }
    else {
      Console.logMessage("SetWindowTitleActionHandler: Window control is null " 
        + pid);
    }
  }        
}