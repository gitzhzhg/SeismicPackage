// VisibleActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.FieldGUIControl;
import com.conoco.cfe.client.gui.controls.GUIControl;

import com.conoco.cfe.utils.ArrayList;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "ModifyField" action.
 */
public class VisibleActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
  /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public VisibleActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
    boolean isVisible = XMLHelper.getBooleanAttributeValue(n, "value");

    GUIControl control = getHelper().getControl(pid, keyword);
    
    if (control != null) {
      control.getComponent().setVisible(isVisible);
      getHelper().getTopLevelWindow(pid).getComponent().invalidate();
      getHelper().getTopLevelWindow(pid).getComponent().validate();
      getHelper().getTopLevelWindow(pid).repaint();
    }
    else {
      //Console.logMessage("VisibleActionHandler: Couldn't find component " + keyword);
    }
  }        
}
