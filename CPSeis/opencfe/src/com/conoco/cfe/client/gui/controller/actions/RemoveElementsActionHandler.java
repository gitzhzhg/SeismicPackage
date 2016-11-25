// RemoveElementsActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.ArrayGUIControl;
import com.conoco.cfe.client.gui.controls.GUIControl;

import com.conoco.cfe.utils.ArrayList;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "RemoveElements" action.
 */
public class RemoveElementsActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
    /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public RemoveElementsActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
    int start = XMLHelper.getIntAttributeValue(n, "startElement")-1;
    int end = XMLHelper.getIntAttributeValue(n, "endElement")-1;
    String keyword = XMLHelper.getUpperCaseStringAttributeValue(n, "keyword");

    GUIControl control = getHelper().getControl(pid, keyword);
    
    if (control != null) {
      ((ArrayGUIControl) control).deleteElements(start, end);
    }
    else {
      //Console.logMessage("RemoveElementsActionHandler: Couldn't find component " + keyword);
    }
  }        
}
