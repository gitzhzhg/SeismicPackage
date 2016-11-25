// ModifyArrayElementActionHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.GUIControlEvent;
import com.conoco.cfe.client.gui.controls.ArrayGUIControl;

import com.conoco.cfe.client.messaging.CommController;

/**
 * An action handler for "ModifyArray" action.
 */
public class ModifyArrayElementActionHandler extends GUIMessageHandlerAdapter {
  /**
   * Constructs a new action handler.
   */
  public ModifyArrayElementActionHandler() {
    super();  
  }
  
  /**
   * Constructs a new action handler with the specified 
   * comms controller.
   * 
   * @param commController   the comms controller to be set on this
   *               this action handler  
   */
  public ModifyArrayElementActionHandler(CommController commController) {
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
    Object src = e.getSource();    
    String value = (String) e.getValue();

    getCommController().transmitMessage(e.getWindowId(), "ModifyArrayElement", ((GUIControl) src).getKeyword(), 
      value, ((ArrayGUIControl) src).getChangedIndex(), ((ArrayGUIControl) src).getChangedIndex());
  }  
}