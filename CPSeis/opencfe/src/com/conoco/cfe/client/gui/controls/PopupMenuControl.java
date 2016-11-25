// PopupMenuControl.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import java.awt.Component;
import java.awt.Font;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;
import javax.swing.SingleSelectionModel;

/**
 * A GUI control that encapsulates the functionality of a 
 * popup menu.
 */
public class PopupMenuControl extends GUIControlAdapter {
  /**
   * Declares a variable for the popup menu component
   * that is contained by this control
   * 
   * @serial
   */
  protected JPopupMenu _popUpMenu;
  
  /**
   * Constructs a new popup menu control.
   */
  public PopupMenuControl() {
    super();
    _popUpMenu = new JPopupMenu();
  }    
  
  /**
   * Constructs a new popup menu control with the specified 
   * parameters.
   * 
   * @param title the title of the popup menu
   */
  public PopupMenuControl(String title) {
    super();
    _popUpMenu = new JPopupMenu(title);
  }
  
  /**
   * Adds the specified menu item control to this control.
   * 
   * @paramm i   the menu item control to be added to this 
   *         popup menu
   */
  public void add(MenuItemControl i) {
    _popUpMenu.add((JMenuItem) i.getComponent());    
  }

  /**
   * Sets the font on this control.
   * 
   * @param f the new font as <code>java.awt.Font</code>
   */
  public void setFont(Font f) {
    _popUpMenu.setFont(f);
  }
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _popUpMenu;
  }
  
  /**
   * Sets the component that will activate this popup menu.
   * 
   * @param c the component that will activte this control
   */
  public void setInvoker(Component c) {
    _popUpMenu.setInvoker(c);
  }
   
  /**
   * Returns the component that activates this popup menu.
   * 
   * @return the popup menu that activates this control
   */
  public Component getInvoker() {
    return _popUpMenu.getInvoker();
  }
  
  /**
   * Adds a separator control to this popup menu control.
   * 
   * @param sc   the separator control to be added to this
   *         control
   */
  public void add(SeparatorControl sc) {
    _popUpMenu.add(sc.getComponent());  
  }
  
  /**
   * Makes this popup menu visible.
   * 
   * @param x the x coordinate of the point where mouse is clicked
   * @param y the y coordinate of the point where mouse is clicked
   */
  public void show(Component c, int x, int y) {
    _popUpMenu.show(c, x, y);
  }
  
  /**
   * Disposes this control.
   */
  public void dispose() {
    _popUpMenu.removeAll();
  }
}