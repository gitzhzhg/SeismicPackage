// GUIMessageHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControlEvent;

/**
 * An interface for an action handler object that is invoked
 * in response to a GUI control event. The main GUI control 
 * listener class has the responsibility of creating the
 * action listeners that are invoked when a GUI control 
 * event takes place. This interface can be implemented 
 * to create handlers for new types of GUI control events.
 * 
 * @see com.conoco.cfe.client.gui.messaging.AppGUIControlListener
 */
public interface GUIMessageHandler {
  
  /** 
   * This method is invoked in response to a GUI control
   * event.
   * 
   * @param e the event object that is generated when 
   *       a GUI control changes state
   */
  public void handleEvent(GUIControlEvent e);  
}