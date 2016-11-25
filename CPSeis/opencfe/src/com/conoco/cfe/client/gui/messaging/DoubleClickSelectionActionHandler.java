// DoubleClickSelectionActionHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.GUIControlEvent;

import com.conoco.cfe.client.messaging.CommController;

/**
 * An action handler for the "SelectionMade" event with 
 * a double click.
 */
public class DoubleClickSelectionActionHandler  extends GUIMessageHandlerAdapter {
  /**
   * Constructs a new action handler.
   */
  public DoubleClickSelectionActionHandler() {
    super();  
  }
  
  /**
   * Constructs a new action handler with the specified comms
   * controller.
   * 
   * @param commController   the comms controller to be set on this
   *               action listener
   */
  public DoubleClickSelectionActionHandler(CommController commController) {
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
    //modified for 109
    getCommController().transmitMessage(e.getWindowId(), "DoubleClicked", 
      src.getKeyword(), value);
  }  
}