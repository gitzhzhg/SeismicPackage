// SensitiveScreenActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.TabPaneControl;

import com.conoco.cfe.utils.ArrayList;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "SensitiveField" action.
 */
public class SensitiveScreenActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
  /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public SensitiveScreenActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
    boolean sensitive = XMLHelper.getBooleanAttributeValue(n, "value");
    
    TabPaneControl tbc = getHelper().getTabPane(pid);

    if (tbc != null) {
      int screen = tbc.getIndexOfScreen(keyword);
      tbc.setSensitive(screen, sensitive);
    }
    else {
      //Console.logMessage("SensitiveScreenActionHandler: Couldn't find component " + keyword);
    }
  }        
}
