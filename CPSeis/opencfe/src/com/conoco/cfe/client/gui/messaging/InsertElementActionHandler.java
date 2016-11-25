// InsertFieldActionHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.GUIControlEvent;

import com.conoco.cfe.client.messaging.CommController;

/**
 * An action handler for the "InsertElement" action.
 */
public class InsertElementActionHandler  extends GUIMessageHandlerAdapter {
  /**
   * Constructs a new action handler.
   */
  public InsertElementActionHandler() {
    super();  
  }
  
  /**
   * Constructs a new action handler with the specified 
   * comms controller.
   * 
   * @param commController   the comms controller that is set
   *               on this action handler
   */
  public InsertElementActionHandler(CommController commController) {
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

    getCommController().transmitMessage(e.getWindowId(), "InsertElement", src.getKeyword(), value);
  }  
}