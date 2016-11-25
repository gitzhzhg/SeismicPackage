// GUIControlEvent.java

package com.conoco.cfe.client.gui.controls;

import java.util.EventObject;

/**
 * Defines the structure of the event that is generated when
 * a GUI control changes state. The event object can be queried
 * for information regarding the control's state.
 */
public class GUIControlEvent  extends EventObject {
  /**
   * Declares a constant for event that is 
   * generated when a field changes value
   * 
   * @serial
   */
  public static final int MODIFY_FIELD_EVENT = 101;
  
  /**
   * Declares a constant for the event that is
   * generated when an array element is modified
   * 
   * @serial
   */
  public static final int MODIFY_ARRAY_ELEMENT_EVENT = 102;
    
  /**
   * Declares a constant for event that is 
   * generated when a window gains focus
   * 
   * @serial
   */
  public static final int WINDOW_FOCUS_EVENT = 103;
      
  /**
   * Declares a constant for event that is 
   * generated when an array component loses focus
   * 
   * @serial
   */
  public static final int LEAVE_ARRAY_EVENT = 104;

  /**
   * Declares a constant for event that is 
   * generated when an arrayset component loses focus
   * 
   * @serial
   */
  public static final int LEAVE_ARRAYSET_EVENT = 105;
  
  /**
   * Declares a constant for event that is 
   * generated when a screen component loses focus
   * 
   * @serial
   */
  public static final int LEAVE_SCREEN_EVENT = 106;
  
  /**
   * Declares a constant for event that is 
   * generated when a selection is made in a control
   * 
   * @serial
   */
  public static final int ITEMS_SELECTED_EVENT = 107;
  
  /**
   * Declares a constant for event that is 
   * generated when a selection is made in a control
   * (a double mouse click)
   * 
   * @serial
   */
  public static final int DOUBLE_CLICK_SELECTION_EVENT = 108;
  
  /**
   * Declares a constant for event that is 
   * generated when a button control is pressed
   * 
   * @serial
   */
  public static final int BUTTON_PRESS_EVENT = 109;

  /**
   * Declares a variable for the event that is generated
   * when a component gains focus.
   * 
   * @serial
   */
  public static final int COMPONENT_FOCUS_EVENT = 110;
  
  /**
   * Declares a variable for the event that is generated
   * when a window requests to close
   * 
   * @serial
   */
  public static final int WINDOW_CLOSING_EVENT = 111;
  
  /**
   * Declares a variable for the event that is generated
   * when elements are to be inserted in an array component
   * 
   * @serial
   */
  public static final int INSERT_ELEMENT_EVENT = 112;

  /**
   * Declares a variable for the event that is generated
   * when elements are to be removed from an array component
   * 
   * @serial
   */
  public static final int REMOVE_ELEMENT_EVENT = 113;
  
  /**
   * Declares a variable for the event that is generated
   * when an item in an array component is clicked
   * 
   * @serial
   */
  public static final int ITEM_CLICKED_EVENT = 114;
  
  /**
   * Declares a variable for the event that is generated
   * when elements are to be pasted in an array component
   *
   * @serial
   */
  public static final int PASTE_ELEMENTS_EVENT = 115;

  /**
   * Declares a variable for the event that is generated
   * when control detects a mouse entered event
   *
   * @serial
   */
  public static final int MOUSE_ENTERED_EVENT = 116;

  /**
   * Declares a variable for the event that is generated
   * when a screen gains focus
   *
   * @serial
   */
  public static final int ENTER_SCREEN_EVENT = 117;       
  
  /**
   * Declares a variable for the event that is generated
   * when control detects a key pressed event
   *
   * @serial
   */
  public static final int KEY_PRESS_EVENT = 118;
     
  /**
   * Variable for the type of this event
   * 
   * @serial
   */
  protected int _type;
  
  /**
   * Declares a variable for storing the value of this event.
   * 
   * @serial
   */
  protected Object _value;

  /**
   * Declares the variable for the window id.
   * 
   * @serial
   */
  protected int _winId;
  
  /**
   * Constructs a new event object.
   * 
   * @param source the object that generates the event
   * @param type the type of the event
   * @param value the value of the event object
   */
  public GUIControlEvent(Object source, int type, Object value) {
    super(source);
    _type = type;
    _value = value;
  }
  
  /**
   * Returns the type of this event object.
   * 
   * @return the type of this event
   */
  public int getType() {
    return _type;
  }
  
  /**
   * Returns the value of this event object.
   * 
   * @return the value of this event object
   */
  public Object getValue() {
    return _value;
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
}