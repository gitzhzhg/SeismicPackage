// GUIControlListener.java

package com.conoco.cfe.client.gui.controls;

/**
 * An interface that can be implemented to receive
 * a notification when a GUI control changes its state.
 * 
 * @see com.conoco.cfe.client.guicontrol.GUIControl
 * @see com.conoco.cfe.client.guicontrol.GUIControlEvent
 */
public interface GUIControlListener {
  /**
   * This method is invoked by a GUI control
   * object when its state changes. 
   * 
   * @param e the GUI control event that is generated
   *       when a GUI control changes state
   * @exception 
   */
  public void guiControlChanged(GUIControlEvent e) throws GUIControlException;  
}