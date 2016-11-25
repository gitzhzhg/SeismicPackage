// FieldGUIControl.java

package com.conoco.cfe.client.gui.controls;

/**
 * Defines the interface for a GUI control that is a field.
 * A field control has a value associated with it and, 
 * optionally, an array of items. 
 */
public interface FieldGUIControl extends GUIControl {
  /**
   * Sets the value on this field. 
   * 
   * @param s the string that describes the value to be set on this control
   */
  public void setValue(String s);
  
  /**
   * Sets the items on this control. This method is applicable 
   * to fields such as combo-boxes. Invocation of this method 
   * on fields such as integer-formatted text fields has no effect.
   * 
   * @param items the items to be set on this control as an
   *         array of <code>java.lang.String</code>    
   */
  public void setItems(String[] items);
}