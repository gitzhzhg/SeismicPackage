// EnterScreenActionHandler.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControlEvent;
import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.TabPaneControl;

import com.conoco.cfe.client.messaging.CommController;

import java.util.Hashtable;

/**
 * An action handler for the "EnterScreen" message.
 */
public class EnterScreenActionHandler  extends GUIMessageHandlerAdapter {
  /**
   * Constructs a new action handler.
   */
  public EnterScreenActionHandler() {
    super();  
  }
  
  /**
   * Constructs a new action handler with the specified comms 
   * controller.
   * 
   * @param commController   the comms controller to be set on this
   *               adapter
   */
  public EnterScreenActionHandler(CommController commController) {
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
    TabPaneControl tbc = (TabPaneControl) (e.getSource());
    Hashtable h = tbc.getIndexScreenKeyLookup();

    getCommController().transmitMessage(e.getWindowId(), "EnterScreen", 
      ((GUIControl) (h.get(e.getValue()))).getKeyword(), null);
  }  
}