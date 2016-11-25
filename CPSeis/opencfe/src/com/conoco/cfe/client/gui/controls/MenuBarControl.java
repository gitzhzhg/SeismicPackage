// MenuBarControl.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import java.awt.Component;
import java.awt.Font;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.JMenuBar;

/**
 * A GUI control that encapsulates a menubar. 
 */
public class MenuBarControl extends GUIControlAdapter {
  /**
   * Variable for the menubar contained by this control
   * 
   * @serial
   */
  protected JMenuBar _menuBar;
  
  /**
   * Constructs a new menubar control
   */
  public MenuBarControl() {
    super();
    _menuBar = new JMenuBar();
  }
        
  /**
   * Sets the font on this control.
   * 
   * @param f the new font as <code>java.awt.Font</code>
   */
  public void setFont(Font f) {
    _menuBar.setFont(f);
  }
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _menuBar;
  }

  /**
   * Adds the specified component to the menubar.
   * 
   * @param c the component to be added to the menubar
   */        
  public void add(Component c) {
    _menuBar.add(c);  
  }
  
  /**
   * Adds a menu control to the menubar.
   * 
   * @param mic the menu control to be added to the menubar
   */
  public void add(MenuControl mic) {
    _menuBar.add(mic.getComponent());  
  }
  
  /**
   * Disposes this control.
   */
  public void dispose() {
    _menuBar.removeAll();
  }          
}