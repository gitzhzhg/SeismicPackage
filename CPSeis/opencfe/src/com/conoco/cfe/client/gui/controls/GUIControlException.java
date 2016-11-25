// GUIControlException.java

package com.conoco.cfe.client.gui.controls;

/**
 * Defines a general exception that is thrown when
 * control's state change is rejected by the server.
 */
public class GUIControlException  extends Exception {
  /**
   * Declares a variable to store the GUI control
   * that generates the exception
   * 
   * @serial
   */
  protected GUIControl _control;
  
  /**
   * Constructs a a new exception
   */  
  public GUIControlException() {
    super();
  }    
  
  /**
   * Constructs a new exception object with the specified message.
   * 
   * @param s the message that describes the exception
   */  
  public GUIControlException(String s) {
    super(s);
  }    
  
  /**
   * Sets the GUI control that generates the exception
   * 
   * @param control the GUI control generating the exception
   */
  public void setGUIControl(GUIControl control) { 
    _control = control;  
  }

  /**
   * Returns the GUI control that generates the exception
   * 
   * @param control the GUI control generating the exception
   */  
  public GUIControl getGUIControl() {
    return _control;
  }
}