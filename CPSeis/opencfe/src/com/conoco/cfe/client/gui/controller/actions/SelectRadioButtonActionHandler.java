// SelectRadioButtonActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.ToggleButtonControl;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "SelectRadioButton" action.
 */
public class SelectRadioButtonActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
  /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public SelectRadioButtonActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
    
    GUIControl control = getHelper().getControl(pid, keyword);
    
    if ((control != null) && (control instanceof ToggleButtonControl)) {
      com.conoco.cfe.utils.EventQueue.invokeLater(
        new RequestFocusRunnable(pid, (ToggleButtonControl) control));
    }
    else {
      //Console.logMessage("SelectRadioButtonActionHandler: Couldn't find component " + keyword);
    }
  }        
   
   /**
    * Inner class that implements the runnable interface to 
    * select a specified radio button.
    */
   class RequestFocusRunnable implements Runnable {
     int _pid;
     ToggleButtonControl _control;
     
     public RequestFocusRunnable(int pid, ToggleButtonControl control) {
       _pid = pid;
       _control = control;    
     }
     
     /**
      * This method is called when the new thread is spawned.
      */
     public void run() {
      _control.setSelected(true);
     }  
   }
}
