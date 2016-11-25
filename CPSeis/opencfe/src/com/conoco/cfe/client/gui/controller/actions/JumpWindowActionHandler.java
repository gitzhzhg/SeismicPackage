// JumpWindowActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.WindowControl;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

import java.awt.Window;

/**
 * Action handler for the "JumpWindow" action.
 */
public class JumpWindowActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
     /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public JumpWindowActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
  
    WindowControl control = getHelper().getTopLevelWindow(pid);
    
    if (control != null) {
      com.conoco.cfe.utils.EventQueue.invokeLater(
        new RequestFocusRunnable(control));
    }
    else {
      //Console.logMessage("JumpWindowActionHandler: Couldn't find window " + pid);
    }
  }        
   
   /**
    * Inner class that implements the runnable interface to 
    * divert the focus to the array component.
    */
   class RequestFocusRunnable implements Runnable {
    WindowControl _control;
         
     public RequestFocusRunnable(WindowControl c) {
       _control = c;
     }
     
     /**
      * This method is called when the new thread is spawned.
      */
     public void run() {
      _control.requestFocus(0);
      ((Window) _control.getComponent()).toFront();
     }  
   }
}
