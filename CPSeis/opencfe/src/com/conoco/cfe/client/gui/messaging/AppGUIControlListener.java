// AppGUIControlListener.java

package com.conoco.cfe.client.gui.messaging;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.GUIControlListener;
import com.conoco.cfe.client.gui.controls.GUIControlEvent;

import java.util.Hashtable;

/**
 * Implements the state change listener for all the GUI controls
 * on the client side. The GUI control listener is responsible
 * for transmitting the GUI events to the comm module. The listener 
 * delegates the transmittal of these messages to action handler objects
 * which are created in advance.
 * 
 * @see com.conoco.cfe.client.gui.messaging.GUIMessageHandler
 * @see com.conoco.cfe.client.messaging.CommModule
 */
public class AppGUIControlListener  implements GUIControlListener {
  /**
   * A collection of handlers that handle the different GUI
   * control events
   *
   * @serial
   */
  protected static Hashtable _handlers;
  
  /**
   * Declares the variable for the window id.
   * 
   * @serial
   */
  protected int _winId;
  
  /**
   * Constructs a new GUI event listener.
   */
  public AppGUIControlListener() {
  }
  
  /**
   * Sets the window id.
   * 
   * @param winId the window id to be set
   */
  public void setWindowId(int winId) {
    _winId = winId;  
  }
  
  /**
   * Returns the window id.
   * 
   * @return the window id
   */
  public int getWindowId() {
    return _winId;
  }
  
  /**
   * Creates the action handlers for the events. 
   * Protected method called by the constructor.
   */
   protected static void createActionHandlers() {
    _handlers = new Hashtable();
    
    GUIMessageHandlerAdapter listener;    //SMCook rearranged for readability

    listener = new ModifyFieldActionHandler();
    _handlers.put( new Integer(GUIControlEvent.MODIFY_FIELD_EVENT), listener);
    
    listener = new ModifyArrayElementActionHandler();
    _handlers.put( new Integer(GUIControlEvent.MODIFY_ARRAY_ELEMENT_EVENT), listener);

    listener = new WindowFocusActionHandler();
    _handlers.put( new Integer(GUIControlEvent.WINDOW_FOCUS_EVENT), listener);
    
    listener = new LeaveArrayActionHandler();
    _handlers.put( new Integer(GUIControlEvent.LEAVE_ARRAY_EVENT), listener);

    listener = new LeaveArraySetActionHandler();
    _handlers.put( new Integer(GUIControlEvent.LEAVE_ARRAYSET_EVENT), listener);

    listener = new LeaveScreenActionHandler();
    _handlers.put( new Integer(GUIControlEvent.LEAVE_SCREEN_EVENT), listener);

    listener = new ItemsSelectedActionHandler();
    _handlers.put( new Integer(GUIControlEvent.ITEMS_SELECTED_EVENT), listener);

    listener = new DoubleClickSelectionActionHandler();
    _handlers.put( new Integer(GUIControlEvent.DOUBLE_CLICK_SELECTION_EVENT), listener);
  
    listener = new ButtonPressActionHandler();
    _handlers.put( new Integer(GUIControlEvent.BUTTON_PRESS_EVENT), listener);        
    
    listener = new WindowClosingActionHandler();
    _handlers.put( new Integer(GUIControlEvent.WINDOW_CLOSING_EVENT), listener);
      
    listener = new InsertElementActionHandler();
    _handlers.put( new Integer(GUIControlEvent.INSERT_ELEMENT_EVENT), listener);

    listener = new RemoveElementActionHandler();
    _handlers.put( new Integer(GUIControlEvent.REMOVE_ELEMENT_EVENT), listener);

    listener = new ItemClickedActionHandler();
    _handlers.put( new Integer(GUIControlEvent.ITEM_CLICKED_EVENT), listener);

    listener = new PasteElementsActionHandler();
    _handlers.put( new Integer(GUIControlEvent.PASTE_ELEMENTS_EVENT), listener);
    
    listener = new EnterScreenActionHandler();
    _handlers.put( new Integer(GUIControlEvent.ENTER_SCREEN_EVENT), listener);

    listener = new KeyPressActionHandler();
    _handlers.put( new Integer(GUIControlEvent.KEY_PRESS_EVENT), listener);
  }
  
  /**
   * Returns the action handlers that handle the GUI events.
   *
   * @return the action handlers that handle the events
   */
  public static Hashtable getActionHandlers() {
    if ( _handlers == null ) {
      createActionHandlers();
    }
    return _handlers;  
  }
  
  /**
   * This method is invoked when a control changes state.
   * 
   * @param e the event object that is generated when the GUI
   *       control changes state
   */
  public void guiControlChanged(GUIControlEvent e) {
    e.setWindowId(getWindowId());
    GUIMessageHandler handler = (GUIMessageHandler) getActionHandlers().get(
      new Integer(e.getType()));
    if ( handler != null ) {
      handler.handleEvent(e);    
    }
  }
}
