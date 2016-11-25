// OptionsFieldActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.FieldGUIControl;
import com.conoco.cfe.client.gui.controls.GUIControl;

import com.conoco.cfe.utils.ArrayList;

import com.conoco.xml.StringArray;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "OptionsField" action.
 */
public class OptionsFieldActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
  /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public OptionsFieldActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
    String start = XMLHelper.getStringAttributeValue(n, "startElement");
    String end = XMLHelper.getStringAttributeValue(n, "endElement");
    String keyword = XMLHelper.getUpperCaseStringAttributeValue(n, "keyword");
    String els = XMLHelper.getStringAttributeValue(n, "elements");
    
    String[] array = StringArray.parseStringArray(els);
    
    GUIControl control = getHelper().getControl(pid, keyword);
    
    if (control != null) {
      ((FieldGUIControl) control).setItems(array);
    }
    else {
      //Console.logMessage("OptionsFieldActionHandler: Couldn't find component " + keyword);
    }
  }        
}
