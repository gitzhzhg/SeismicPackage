// ClearElementsActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.ArrayGUIControl;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.xml.StringArray;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * An action handler for the "ClearElements" reply.
 */
public class ClearElementsActionHandler 
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
     
   /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public ClearElementsActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
    String keyword = XMLHelper.getUpperCaseStringAttributeValue(n, "keyword");
        
    ArrayGUIControl arrayControl = (ArrayGUIControl) getHelper().getControl(pid, keyword);
    
    if ( arrayControl != null ) {
      arrayControl.deleteElements(0, arrayControl.getRowCount()-1);
    } 
    else {
      //Console.logMessage("ClearElementsActionHandler: Couldn't find component " + keyword);  
    }  
  }        
}
