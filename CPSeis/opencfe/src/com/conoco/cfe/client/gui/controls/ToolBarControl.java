// ToolBarControl.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import java.awt.Component;
import java.awt.Font;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.JToolBar;

/**
 * A GUI control that encapsulates a toolbar component.
 */
public class ToolBarControl extends GUIControlAdapter {
  /**
   * Variable for the toolbar component contained 
   * by this control
   * 
   * @serial
   */
  protected JToolBar _toolBar;
  
  /**
   * Constructs a new toolbar control.
   */
  public ToolBarControl() {
    super();
    _toolBar = new JToolBar();
  }
        
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _toolBar;
  }
  
  /**
   * Adds a button control object to the toolbar component.
   * 
   * @param mic the button control to be added to the toolbar
   */
  public void add(ButtonControl mic) {
    _toolBar.add(mic.getComponent());  
  }
  
  /** 
   * Adds the separator control to 
   */
  public void add(SeparatorControl sc) {
    _toolBar.add(sc.getComponent());  
  }
  
  /**
   * Disposes this control.
   */
  public void dispose() {
    _toolBar.removeAll();
    super.dispose();
  }                
}