// WindowClosingActionHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.WindowControl;
import com.conoco.cfe.client.gui.controls.GUIControlEvent;

import com.conoco.cfe.client.messaging.CommController;

/**
 * An action handler for "WindowClosing" action.
 */
public class WindowClosingActionHandler  extends GUIMessageHandlerAdapter {

  /**
   * Constructs a new action handler.
   */
  public WindowClosingActionHandler() {
    super();  
  }
  
  /**
   * Constructs a new action handler with the specified comms
   * controller.
   * 
   * @param commController   the comms controller to be set on this
   *               adapter
   */
  public WindowClosingActionHandler(CommController commController) {
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
    WindowControl src = (WindowControl) e.getSource();    
    if (e.getWindowId() == 1) {
      getCommController().transmitMessage(e.getWindowId(), "TerminateApp", null, null);
    }
    else {
      getCommController().transmitMessage(e.getWindowId(), "CloseWindow", src.getKeyword(), null);
    }  
  }  
}