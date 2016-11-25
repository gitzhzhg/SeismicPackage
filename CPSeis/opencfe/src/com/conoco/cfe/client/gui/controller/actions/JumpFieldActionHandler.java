// JumpFieldActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.GUIControl;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "JumpFieldAction" action.
 */
public class JumpFieldActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
    /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public JumpFieldActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
    
    if (control != null) {
      com.conoco.cfe.utils.EventQueue.invokeLater(
        new RequestFocusRunnable(pid, control));
    }
    else {
      //Console.logMessage("JumpFieldActionHandler: Couldn't find component " + keyword);
    }
  }        
   
   /**
    * Inner class that implements the runnable interface to 
    * divert the focus to the array component.
    */
   class RequestFocusRunnable implements Runnable {
     int _pid;
     GUIControl _control;
     
     public RequestFocusRunnable(int pid, GUIControl control) {
       _pid = pid;
       _control = control;    
     }
     
     /**
      * This method is called when the new thread is spawned.
      */
     public void run() {
      _control.requestFocus(0);
     }  
   }
}
