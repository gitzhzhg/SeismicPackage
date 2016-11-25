///
/// ButtonPressActionHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4. 07-22-2004 SMCook   Introduced _mostRecentKeyword to help distinguish
///                          GUI state (e.g. for setEditable logic) in class
///                          AppGUIController.
///  3. 09-29-2003 SMCook   Introduced _mostRecentWindowId variable as part of
///                          a scheme allowing for adequate parent/child
///                          tracking, which in turn allows for sensible
///                          which-window-is-on-top behavior.
///

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.GUIControlEvent;

import com.conoco.cfe.client.messaging.CommController;

/**
 * An action handler for "ButtonPress" action.
 */
public class ButtonPressActionHandler  extends GUIMessageHandlerAdapter {

  private static int    _mostRecentWindowId = -1;
  private static String _mostRecentKeyword;

  /**
   * Constructs a new action handler.
   */
  public ButtonPressActionHandler() {
    super();  
  }
  
  /**
   * Constructs a new action handler with the specified 
   * comms controller.
   * 
   * @param commController   the comms controller to be set on this
   *               action listener
   */
  public ButtonPressActionHandler(CommController commController) {
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
    
    _mostRecentWindowId = e.getWindowId();
    _mostRecentKeyword = src.getKeyword();

    getCommController().transmitMessage(
      _mostRecentWindowId, "ButtonPress", _mostRecentKeyword, null);
  }  

  /**
   * Part of scheme allowing for parent/child tracking.
   */
  public static final int getMostRecentWindowId() {
    return _mostRecentWindowId;
  }

  /**
   * Part of scheme allowing for GUI state tracking.
   */
  public static final String getMostRecentKeyword() {
    return _mostRecentKeyword;
  }
}
