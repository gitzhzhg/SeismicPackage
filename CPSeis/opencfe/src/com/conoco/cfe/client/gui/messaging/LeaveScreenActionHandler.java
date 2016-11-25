// LeaveScreenActionHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControlEvent;
import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.TabPaneControl;

import com.conoco.cfe.client.messaging.CommController;

/**
 * An action handler for the "LeaveScreen" message.
 */
public class LeaveScreenActionHandler  extends GUIMessageHandlerAdapter {
  /**
   * Constructs a new action handler.
   */
  public LeaveScreenActionHandler() {
    super();  
  }
  
  /**
   * Constructs a new action handler with the specified comms 
   * controller.
   * 
   * @param commController   the comms controller to be set on this
   *               adapter
   */
  public LeaveScreenActionHandler(CommController commController) {
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
    getCommController().transmitMessage(e.getWindowId(), "LeaveScreen", 
      ((TabPaneControl) e.getSource()).getKeywordOfLastScreen(), null);
  }  
}