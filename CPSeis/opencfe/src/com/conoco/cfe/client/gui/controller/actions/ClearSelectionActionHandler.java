// ClearSelectionActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.ArrayGUIControl;
import com.conoco.cfe.client.gui.controls.ArrayGUIControlAdapter;
import com.conoco.cfe.client.gui.controls.GUIControl;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.utils.ArrayList;

import com.conoco.xml.StringArray;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "ModifyElements" action.
 */
public class ClearSelectionActionHandler
 extends ReplyActionHandlerAdapter implements XmlActionHandler {
 

  /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public ClearSelectionActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
    String keyword = XMLHelper.getUpperCaseStringAttributeValue(n, "keyword");
  
    ArrayGUIControl arrayControl = (ArrayGUIControl) getHelper().getControl(pid, keyword);
    
    if (arrayControl != null) {
      arrayControl.clearSelection();
    }
    else {
      //Console.logMessage("ClearSelectionActionHandler: Couldn't find component " + keyword);
    }
  }        
}
 
 
