// WindowFocusActionHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.WindowControl;
import com.conoco.cfe.client.gui.controls.GUIControlEvent;

import com.conoco.cfe.client.messaging.CommController;

/**
 * An action handler for "WindowFocus" action.
 */
public class WindowFocusActionHandler  extends GUIMessageHandlerAdapter {
  /**
   * Constructs a new action handler.
   */
  public WindowFocusActionHandler() {
    super();  
  }
  
  /**
   * Constructs a new action handler with the specified comms
   * controller.
   * 
   * @param commController   the comms controller to be set on this
   *               adapter
   */
  public WindowFocusActionHandler(CommController commController) {
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
    getCommController().transmitMessage(e.getWindowId(), "EnterWindow", src.getKeyword(), null);
  }  
}