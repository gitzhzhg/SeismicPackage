// LeaveArraySetActionHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.GUIControlEvent;

import com.conoco.cfe.client.messaging.CommController;

/**
 * An action handler for the "LeaveArray" action.
 */
public class LeaveArraySetActionHandler  extends GUIMessageHandlerAdapter {
  /**
   * Constructs a new action handler.
   */
  public LeaveArraySetActionHandler() {
    super();  
  }
  
  /**
   * Constructs a new action handler with the specified 
   * comms coontroller.
   * 
   * @param commController   the comms controller to be set on this
   *               adapter
   */
  public LeaveArraySetActionHandler(CommController commController) {
    super(commController);
  }
  
  /** 
   * This method is invoked in response to a GUI control
   * event.
   * 
   * @param e the event object that is generated when 
   *       a GUI control changes state
   */
  public void handleEvent(GUIControlEvent e) {
    GUIControl src = (GUIControl) e.getSource();    
    
    getCommController().transmitMessage(e.getWindowId(), "LeaveArraySet", 
      src.getKeyword(), null);
  }  
}