// SeparatorControl.java

package com.conoco.cfe.client.gui.controls;

import java.awt.Component;
import java.awt.Font;

import javax.swing.JSeparator;

/**
 * A GUI control that encapsulates the functionality 
 * of a separator.
 */
public class SeparatorControl extends GUIControlAdapter {
  /**
   * Declares a variable for the separator GUI component.
   * 
   * @serial
   */
  protected JSeparator _separator;
  
  /**
   * Constructs a new separator control.
   */
  public SeparatorControl() {
    _separator = new JSeparator();
  }
  
  /**
   * Constructs a new separator control with the specified orientation.
   * 
   * @param orientation the orientation of the separator
   */
  public SeparatorControl(int orientation) {
    _separator = new JSeparator(orientation);
  }  
  
  /**
   * Returns the GUI component contained by this control.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _separator;
  }
  
  /**
   * Sets the font on this control.
   * 
   * @param f the desired font
   */
  public void setFont(Font f) {
    _separator.setFont(f);
  }
}