// MenuItemControl.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.utils.ArrayList;

import java.awt.Font;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.JMenuItem;

/**
 * A GUI control that encapsulates a menu item. 
 */
public class MenuItemControl extends GUIControlAdapter {
  /**
   * Variable for the menu item that is encapsulated by this
   * control
   * 
   * @serial
   */
  protected JMenuItem _menuItem;
  
  /**
   * Variable for the action listener object
   * 
   * @serial
   */
  protected ActionListener _actionListener;

  /**
   * Constructs a new menu item control.
   */
  public MenuItemControl() {
    super();
    _menuItem = new JMenuItem();
    _actionListener = new ControlVetoer();
    _menuItem.addActionListener(_actionListener);
  }
  
  /**
   * Constructs a new menu item control with the specified text.
   * 
   * @param text the text of the menu item control
   */
  public MenuItemControl(String text) {
    this();
    _menuItem.setText(text);
  }
  
  /**
   * Sets the font on this control.
   * 
   * @param f the new font as <code>java.awt.Font</code>
   */
  public void setFont(Font f) {
    _menuItem.setFont(f);
  }
  
  /**
   */
  public void setValue(String value) {
  }  
  
  /**
   * Sets the sensitivity of this control.
   * 
   * @param sensitive the boolean flag that is true if 
   *           this control is to be sensitive; false otherwise
   */
  public void setSensitive(boolean sensitive) {
    _menuItem.setEnabled(sensitive);  
  }
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public java.awt.Component getComponent() {
    return _menuItem;
  }
  
  /**
   * Diposes this method.
   */
  public void dispose() {
    _menuItem.removeActionListener( _actionListener ) ;
    super.dispose();
  }
  
  /**
   * Inner class that is used to send notification to control
   * listeners when the menu item is selected.
   */
  class ControlVetoer implements ActionListener {
    /**
     * This method is invoked when the menu item  control is 
     * selected.
     * 
     * @param e the event object that is generated when
     *       the button control is clicked
     */
    public void actionPerformed(ActionEvent e) {
      GUIControlEvent event = new 
        GUIControlEvent(MenuItemControl.this, 
          GUIControlEvent.BUTTON_PRESS_EVENT,
            null);
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.out.println("MenuItemControl: Exception");  
      }
    }  
  }  
}