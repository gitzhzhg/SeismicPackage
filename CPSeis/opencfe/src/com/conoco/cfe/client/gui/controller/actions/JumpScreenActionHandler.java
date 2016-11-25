// JumpScreenActionHandler.java

package com.conoco.cfe.client.gui.controller.actions;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.TabPaneControl;

import org.w3c.dom.Node;
import org.w3c.dom.Document;

/**
 * Action handler for the "JumpScreen" action.
 */
public class JumpScreenActionHandler
   extends ReplyActionHandlerAdapter implements XmlActionHandler {
 
    /**
   * Constructs a new action handler.
   * 
   * @param helper the action handler helper
   */
  public JumpScreenActionHandler(AppGUIController.ActionHandlerHelper helper) {
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
    
    TabPaneControl tbc = getHelper().getTabPane(pid);
    
    if (tbc != null) {
      int screen = tbc.getIndexOfScreen(keyword);
      if (screen != -1) {
        com.conoco.cfe.utils.EventQueue.invokeLater(
          new RequestFocusRunnable(pid, tbc, screen));
      }
      else {
        //Console.logMessage("JumpScreenActionHandler: Couldn't find screen " + keyword);
      }
    }
  }        
   
   /**
    * Inner class that implements the runnable interface to 
    * divert the focus to the array component.
    */
   class RequestFocusRunnable implements Runnable {
     int _pid;
     int _screen;
     TabPaneControl _tbc;
     
     public RequestFocusRunnable(int pid, TabPaneControl tbc, int screen) {
       _pid = pid;
       _tbc = tbc;
       _screen = screen;    
     }
     
     /**
      * This method is called when the new thread is spawned.
      */
     public void run() {
       _tbc.requestFocus( _screen );
     }  
   }
}
