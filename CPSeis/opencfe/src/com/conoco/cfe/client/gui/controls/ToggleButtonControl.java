///
/// ToggleButtonControl.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  5.
///  4. 09-24-2001 SMCook   Changed default constructor to set a blank string
///                          rather than a null string in an effort to improve
///                          behavior when running under the XMLViewer.
///


package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.gui.controls.ui.MultiLineLabelToggleButtonUI;

import java.awt.Component;
import java.awt.Font;
import java.awt.Color;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;

import javax.swing.JToggleButton;
import javax.swing.BorderFactory;

/**
 * A GUI control that encapsulates a toggle button component.
 */
public class ToggleButtonControl extends GUIControlAdapter
  implements FieldGUIControl {

  /**
   * A variable for the toggle button that is contained by this
   * control
   * 
   * @serial
   */
  protected JToggleButton _button;
  
  /**
   * Variable for the action listener object
   * 
   * @serial
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
   * Variables pertaining to the list of labels
   *
   * @serial
   */
  protected int _currentIndex;
  protected String[] _labels;
  
  /**
   * Constructs a toggle button control
   */
  public ToggleButtonControl() {
    super();
    _button = new JToggleButton(" ");
    _button.setUI(new MultiLineLabelToggleButtonUI());
    _listener = new ControlVetoer();
    _mListener = new MyMouseListener();
    _button.addActionListener(_listener);
    _button.addMouseListener(_mListener);
  }
  
  /**
   * Constructs a toggle button control with the specified
   * text
   * 
   * @param text the label of the toggle button
   */
  public ToggleButtonControl(String text) {
    this();             //SMCook
    setText(text);      //do not use _button.setText() here
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
   * Sets the state on this toggle button control. This does not fire 
   * a GUI event.
   * 
   * @param b is set to true if the toggle button is to be selected
   */
  public void setSelected(boolean b) {
    _button.setSelected(b);  
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
   * Sets the text on this control.
   * 
   * @param s the label on the button
   */
  public void setText(String s) {   //SMCook modified
    if(_labels==null)
      {
      _button.setText(s);
      return;
      }

    int imax=_labels.length;

//make sure _currentIndex matches value
    int i=0;
    while(i<imax)
      {
      if(s.equals(_labels[i]))
        {
        _currentIndex=i;
        _button.setText(_labels[i]);
        return;
        }
      i++;
      }
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
   * Disposes this object.
   */
  public void dispose() {
    _button.removeActionListener(_listener);
    _button.removeMouseListener(_mListener);
    super.dispose();
  }
 
  /**
   * Sets the button state.
   *
   * @param value the desired state. 
   */
  public void setValue(String value) {     //SMCook
    setText(value);                        //do not use _button.setText() here
  }

  /**
   * Sets the label choices on this toggle button.
   *
   * @param items the labels to be set
   */
  public void setItems(String[] items) {    //SMCook added
    _currentIndex = 0;
    if(items==null) return;

    int imax=items.length;
    if(imax > 2)
      throw new IllegalArgumentException("ToggleButtonControl: error - more than 2 labels");

    _labels=new String[imax];
    int i=0;
    while(i<imax)
      {
      _labels[i]=new String(items[i]);
      i++;
      }
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
   * Inner class that is used to send notification to GUI control
   * listeners when the toggle button control is clicked.
   */
  class ControlVetoer implements ActionListener {
    /**
     * This method is invoked when the toggle button control is 
     * clicked.
     * 
     * @param e the event object that is generated when
     *       the button control is clicked
     */
    public void actionPerformed(ActionEvent e) {       //SMCook
      if(_labels==null) return;

      _currentIndex++;
      if(_currentIndex > _labels.length-1) _currentIndex=0;

      String txt=_labels[_currentIndex];
      setText(txt);

      GUIControlEvent event = new 
        GUIControlEvent(ToggleButtonControl.this, 
          GUIControlEvent.MODIFY_FIELD_EVENT,
            txt);
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.err.println("ToggleButtonControl: Exception");  
      }
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
        GUIControlEvent(ToggleButtonControl.this, 
          GUIControlEvent.MOUSE_ENTERED_EVENT,
            null);
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.err.println("Toggle Button Control: Exception");  
      }
    }  
  }       
}
