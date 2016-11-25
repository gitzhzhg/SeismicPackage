// GUIControl.java

package com.conoco.cfe.client.gui.controls;

/**
 * Defines an interface for a GUI control. The control
 * is wrapper class that encapsulates a standard Swing
 * component. It sends notification events when a GUI event on the component
 * takes place. To listen to these events, 
 * implement <code>com.conoco.cfe.client.gui.controls.GUIControlListener</code>
 * interface and register it with this control.
 * 
 * 
 * @see com.conoco.cfe.client.gui.controls.GUIControlListener
 */
public interface GUIControl {
  
  /**
   * Adds the specified GUI state listener to this control.
   * The listener will be notified when a GUI-related event
   * takes place on this control.
   * 
   * @param l the listener that wants to be notified of a 
   *       GUI-related event on this control
   */
  public void addGUIControlListener(GUIControlListener l);

  /**
   * Removes a specified listener from this control.
   * 
   * @param l the listener that listens to GUI-related events
   *       on this control
   */
  public void removeGUIControlListener(GUIControlListener l);
  
  /**
   * Sets the font on this control.
   * 
   * @param f the desired font
   */
  public void setFont(java.awt.Font f);
   
  /**
   * Sets the sensitivity of this control.
   * 
   * @param sensitive the boolean flag that is true if 
   *           this control is to be sensitive; false otherwise
   */
  public void setSensitive(boolean sensitive);
  
  /**
   * Sets whether this control is editable or not.
   * 
   * @param editable   the boolean flag that is true if
   *           this control is to be editable; false otherwise
   */
  public void setEditable(boolean editable);
  
  /**
   * Posts a request for obtaining focus. 
   * 
   * @param rowNumber this parameter is applicable only to array and arrayset
   *           controls; fields and other controls disregard this parameter
   */
  public void requestFocus(int rowNumber);
  
  /**
   * Sets the keyword that uniquely identifies this control.
   * 
   * @param keyword the desired keyword of this control.
   */
  public void setKeyword(String keyword);
  
  /**
   * Returns the keyword of this control.
   * 
   * @return the unique keyword of this control
   */
  public String getKeyword();
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public java.awt.Component getComponent();
  
  /**
   * Disposes this control.
   */
  public void dispose();
  
  /**
   */
  public void enableKeyBinding(String keyid);
}