///
/// ButtonControl.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-24-2001 SMCook   Changed default constructor to set a blank string
///                          rather than a null string in an effort to improve
///                          behavior when running under the XMLViewer.
///

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.gui.controls.ui.MultiLineLabelButtonUI;

import java.awt.Component;
import java.awt.Font;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;

import javax.swing.JButton;
import javax.swing.BorderFactory;

/**
 * A GUI control for a button component. The button sends
 * a notification to interested listeners when it is clicked.
 */
public class ButtonControl extends GUIControlAdapter {
  /**
   * Variable for the buttons contained by this control
   * 
   * @serial
   */
  protected JButton _button;
  
  /**
   * Variable for the listener object that gets a notification
   * when the button is clicked
   * 
   *  @serial
   */
  protected ActionListener _listener;
    
  /**
   * Variable for the listener object that gets a notification
   * when the button component detects a mouse enter
   *
   * @serial
   */
  protected MouseListener _mListener;
  
  /**
   * Variable for the focus listener
   *
   * @serial
   */
  protected FocusListener _focusListener;
  
  /**
   * Constructs a new button control.
   */
  public ButtonControl() {
    super();
    _button = new JButton(" ");
    _button.setUI(new MultiLineLabelButtonUI());
    _listener = new ControlVetoer();
    _mListener = new MyMouseListener();
    _focusListener = new FocusWatcher();
    _button.addActionListener(_listener);
    _button.addMouseListener(_mListener);
    _button.addFocusListener(_focusListener);
  }
  
  /**
   * Constructs a new button control using the specified
   * label.
   * 
   * @param text the text of the button control
   */
  public ButtonControl(String text) {
    this();
    _button.setText(text);
  }
    
  /**
   * Sets the sensitivity of this control.
   * 
   * @param sensitive the boolean flag that is true if 
   *           this control is to be sensitive; false otherwise
   */
  public void setSensitive(boolean sensitive) {
    _button.setEnabled(sensitive);  
  }
    
  /**
   * Sets a Mnemonic value.
   * 
   * @param shortcut a char specifying the mnemonic value
   */
  public void setMnemonic(char shortcut) {
    _button.setMnemonic(shortcut);  
  }

  /**
   * Posts a request for obtaining focus. 
   * 
   * @param rowNumber this parameter is applicable only to array and arrayset
   *           controls; fields and other controls disregard this parameter
   */
  public void requestFocus(int rowNumber) {
    _button.requestFocus();  
  }
  
  /**
   * Sets the font on this control.
   * 
   * @param f the new font as <code>java.awt.Font</code>
   */
  public void setFont(Font f) {
    _button.setFont(f);
  }
  
  /**
   * Disposes this component.
   */
  public void dispose() {
    _button.removeActionListener(_listener);
    _button.removeMouseListener(_mListener);
    _button.removeFocusListener(_focusListener);
    super.dispose();    
  }
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _button;
  }
    
  /**
   * Inner class that is used to send notification to control
   * listeners when the button control is clicked.
   */
  class ControlVetoer implements ActionListener {
    /**
     * This method is invoked when the button control is clicked.
     * 
     * @param e the event object that is generated when
     *       the button control is clicked
     */
    public void actionPerformed(ActionEvent e) {
      GUIControlEvent event = new 
        GUIControlEvent(ButtonControl.this, 
          GUIControlEvent.BUTTON_PRESS_EVENT,
          null);
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.err.println("ButtonControl: Exception");  
      }
    }  
  }  
  
  /**
   * Inner class that listens to loss and gain of focus on this control.
   */
  class FocusWatcher implements FocusListener {
    /**
     * This method is called when this control gains focus. 
     * 
     * @param e the event that is generated when this control
     *       gains focus
     */
    public void focusGained(FocusEvent e) {
      GUIControlEvent event = new 
        GUIControlEvent(ButtonControl.this, 
          GUIControlEvent.COMPONENT_FOCUS_EVENT,
          null);
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
      System.err.println("Button Focus Exception");  
      }
    }

    /**
     * This method is called when this control loses focus. 
     * 
     * @param e the event that is generated when this control
     *       loses focus
     */        
    public void focusLost(FocusEvent e) {
      
    }
  }    
  
  /**
   * Inner class that implements a mouse listener to listen 
   * to mouse entered events on the button.
   */      
  class MyMouseListener extends MouseAdapter {
    /**
     * Method that is invoked when the button detects a mouse enter.
     */
    public void mouseEntered(MouseEvent e) {
      GUIControlEvent event = new 
        GUIControlEvent(ButtonControl.this, 
          GUIControlEvent.MOUSE_ENTERED_EVENT,
          null);
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.err.println("ButtonControl: Exception");  
      }
    }
  }
}
