// RemoveElementActionHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.GUIControlEvent;

import com.conoco.cfe.client.messaging.CommController;

/**
 * An action handler for the "RemoveElement" action.
 */
public class RemoveElementActionHandler  extends GUIMessageHandlerAdapter {
  /**
   * Constructs a new action handler.
   */
  public RemoveElementActionHandler() {
    super();
  }
  
  /**
   * Constructs a new action handler with the specified 
   * comms controller.
   * 
   * @param commController   the comms controller that is set
   *               on this action handler
   */
  public RemoveElementActionHandler(CommController commController) {
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
    String value = (String) e.getValue();

    getCommController().transmitMessage(e.getWindowId(), "RemoveElement", src.getKeyword(), value);
  }  
}