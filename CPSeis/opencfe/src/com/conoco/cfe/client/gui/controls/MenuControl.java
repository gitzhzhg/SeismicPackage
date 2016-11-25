// MenuControl.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import java.awt.Component;
import java.awt.Font;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

/**
 * A GUI control for a menu.
 */
public class MenuControl extends GUIControlAdapter {
  /**
   * Variable for the menu contained by this control.
   * 
   * @serial
   */
  protected JMenu _menu;
  
  /**
   * Constructs a new menu control.
   */
  public MenuControl() {
    super();
    _menu = new JMenu();
  }

  /**
   * Constructs a new menu control with the specified text.
   * 
   * @param text the text of the menu control
   */
  public MenuControl(String text) {
    super();
    _menu = new JMenu(text);
  }
         
  /**
   * Sets the font on this control.
   * 
   * @param f the new font as <code>java.awt.Font</code>
   */
  public void setFont(Font f) {
    _menu.setFont(f);
  }
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _menu;
  }
  
  /**
   * Adds the specified component to the menubar.
   * 
   * @param c the component to be added to the menubar
   */        
  public void add(Component c) {
    _menu.add(c);  
  }
  
  /**
   * Adds the specified menu item control to the menubar.
   * 
   * @param mic the menu item control to be added
   */
  public void add(MenuItemControl mic) {
    this.add(mic.getComponent());  
  }
  
  /**
   * Adds the separator control to this menu.
   * 
   * @param sc the separator control to be added
   */
  public void add(SeparatorControl sc) {
    this.add(sc.getComponent());  
  }    
  
  /**
   * Adds the label control to this menu.
   * 
   * @para, lc the label control to be added
   */
  public void add(LabelControl lc) {
    this.add(lc.getComponent());  
  }
  
  /**
   * Disposes this control.
   */
  public void dispose() {
    _menu.removeAll();  
  }
}