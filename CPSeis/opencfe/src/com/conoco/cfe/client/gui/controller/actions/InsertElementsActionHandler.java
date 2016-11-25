// InsertElementsActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.ArrayGUIControl;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.xml.StringArray;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "InsertElements" action.
 */
public class InsertElementsActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
   /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public InsertElementsActionHandler(AppGUIController.ActionHandlerHelper helper) {
    super(helper);  
  }

  /**
   * Performs the action as specified by the XML node describing the action.
   * This method is invoked by the message decoder. The execution of 
   * the action is delegated to the state controller.
   * 
   * @param n the XML node for the message
   */  
  public void performAction(Node n) {
    int pid = XMLHelper.getIntAttributeValue(n, "windowId");
    int start = XMLHelper.getIntAttributeValue(n, "startElement")-1;
    String keyword = XMLHelper.getUpperCaseStringAttributeValue(n, "keyword");
    String elements = XMLHelper.getStringAttributeValue(n, "elements");
    
    String[] array = StringArray.parseStringArray(elements);
    
    ArrayGUIControl arrayControl = (ArrayGUIControl) 
      getHelper().getControl(pid, keyword);
    
    if (arrayControl != null) {
      arrayControl.insertElements(array, start);
    }
    else {
      Console.logMessage("InsertElements: Failed to find component " + keyword);
    }
  }        
}