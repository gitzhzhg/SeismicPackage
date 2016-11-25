//PasteElementsActionHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.GUIControlEvent;

import com.conoco.cfe.client.messaging.CommController;

/**
 * An action handler for the "PasteElements" reply message.
 */
public class PasteElementsActionHandler extends GUIMessageHandlerAdapter {
  /**
   * Constructs a new action handler.
   */
  public PasteElementsActionHandler() {
    super();  
  }
  
  /**
   * Constructs a new action handler with the specified comms
   * controller.
   * 
   * @param commController   the comms controller to be set on this
   *               action listener
   */
  public PasteElementsActionHandler(CommController commController) {
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
    Object[] values = (Object[]) e.getValue();
    
    StringBuffer buffer = new StringBuffer();
    
    for ( int i = 0; i < values.length; i++ ) {
      buffer.append(values[i]);
      buffer.append("\n");
    }
        
    // The reason why 1 is subtracted from the length is that
    // in the XMLEncoder it normalizes indices to go from 1 to N
    // (ie it adds 1)
    getCommController().transmitMessage(e.getWindowId(), "PasteElements", 
      src.getKeyword(), buffer.toString(), 0, values.length -1);
  }  
}