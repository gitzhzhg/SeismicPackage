// LabelControl.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.gui.controls.ui.MultiLineLabelUI;

import java.awt.Component;
import java.awt.Font;

import javax.swing.JLabel;
import javax.swing.SwingConstants;

/**
 * A GUI control that encapsulates a label. 
 */
public class LabelControl extends GUIControlAdapter {
  /**
   * The label contained by this control
   * 
   * @serial
   */
  protected JLabel _label;
    
  /**
   * Constructs a new label control
   */
  public LabelControl() {
    super();
    _label = new JLabel();
    _label.setUI(new MultiLineLabelUI());
  }
  
  /**
   * Constructs a new label control with the specified text.
   * 
   * @param text the text of the label
   */
  public LabelControl(String text) {
    this();
    _label.setText(text);
  }
  
  /**
   * Set the horizontal alignment for the label
   */
  public void setAlignment(String align) {
    if (align.equals("left")) {
      _label.setHorizontalAlignment(SwingConstants.LEFT);
    }
    else if (align.equals("right")) {
      _label.setHorizontalAlignment(SwingConstants.RIGHT);
    }
    else {
      _label.setHorizontalAlignment(SwingConstants.CENTER);
    }
  }    
  
  /**
   * Sets the font on this control.
   * 
   * @param f the new font as <code>java.awt.Font</code>
   */
  public void setFont(Font f) {
    _label.setFont(f);
  }
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _label;
  }
}