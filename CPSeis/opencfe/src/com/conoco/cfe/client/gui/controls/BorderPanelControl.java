// BorderPanelControl.java

package com.conoco.cfe.client.gui.controls;

import java.awt.Component;

import javax.swing.JPanel;
import javax.swing.border.Border;

/**
 * A control to implement borders. 
 */
public class BorderPanelControl extends GUIControlAdapter {
  /**
   * Variable for the panel that will be used to draw the border
   * 
   * @serial
   */  
  protected JPanel _panel;
  
  /**
   * Creates a new border panel control. 
   */
  public BorderPanelControl() {
    super();
    _panel = new JPanel();
  }  
  
  /**
   * Returns the component contained by this control.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _panel;
  }
  
  /**
   * Sets the border object.
   * 
   * @param b the border to be set on this control
   */
  public void setBorder(Border b) {
    _panel.setBorder(b);  
  }
  
  /**
   * Sets the opacity on this component.
   * 
   * @param opacity   a boolean variable that is set to <code>true</code>
   *           if this component is to be opaque; <code>false</code>
   *           otherwise
   */
  public void setOpaque(boolean opacity) {
    _panel.setOpaque(opacity);  
  }
}