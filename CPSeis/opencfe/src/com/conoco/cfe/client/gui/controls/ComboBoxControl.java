///
/// ComboBoxControl.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 02-07-2002 SMCook   Added a _comboBox.setMaximumRowCount(15) call to
///                          keep unnecessary vertical scroll bar from popping
///                          up in short drop down lists.
///

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import java.awt.Component;
import java.awt.Font;
import java.awt.Color;

import java.awt.event.MouseEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.beans.PropertyChangeEvent;

import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.BorderFactory;

import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.metal.MetalComboBoxUI;
import javax.swing.plaf.ComponentUI;

/**
 * A GUI control that encapsulates the functionality of a 
 * combo box.
 */
public class ComboBoxControl extends GUIControlAdapter
  implements FieldGUIControl {
  
  /**
   * Declares the combo box component contained by this control.
   * 
   * @serial
   */
  protected ConocoComboBox _comboBox;
  
  /**
   * Declares a boolean flag that is set if the elements of the
   * combo box are set externally. This is required so that 
   * the control doesnt send the (same) message back to the 
   * server again.
   *
   * @serial
   */
  protected boolean SET_VALUE_FLAG;
  
  /**
   * Declares a variable for the focus listener
   * 
   * @serial
   */
  protected FocusListener _focusListener;
  
  /**
   * Declares a variable for the item listener
   * 
   * @serial
   */
  protected ItemListener _itemListener;
  
  /**
   * Variable for the listener object that gets a notification
   * when the button component detects a mouse enter
   *
   * @serial
   */
  protected MouseListener _mListener;
  
  /**
   * Declares a boolean for sensitivity
   * 
   * @serial
   */
  protected boolean _isSensitive;

  /**
   */
  static {
    UIDefaults def = UIManager.getDefaults();
    def.put("ComboBox.border", 
      BorderFactory.createEtchedBorder());    
  }
          
  /**
   * Constructs a new combo box control.
   */      
  public ComboBoxControl() {
    super();
    _comboBox = new ConocoComboBox();
    _comboBox.setModel(new ConocoComboBoxModel());
    _comboBox.setUI(new MyComboBoxUI());
    _focusListener = new FocusWatcher();
    _itemListener = new SelectionListener();
    _mListener = new MyMouseListener();
    _comboBox.addFocusListener(_focusListener);
    _comboBox.addItemListener(_itemListener);
    _comboBox.addMouseListener(_mListener);
  }
       
  /**
   * Sets the font on this control.
   * 
   * @param f the desired font
   */
  public void setFont(Font f) {
    _comboBox.setFont(f);
  }

  /**
   * Sets the sensitivity of this control.
   * 
   * @param sensitive the boolean flag that is true if 
   *           this control is to be sensitive; false otherwise
   */
  public void setSensitive(boolean sensitive) {
    _comboBox.setEnabled(sensitive);
    _isSensitive = sensitive;
  }
  
  /**
   * Sets whether this control is editable or not.
   * 
   * @param editable   the boolean flag that is true if
   *           this control is to be editable; false otherwise
   */
  public void setEditable(boolean editable) {
  }
  
  /**
   * Posts a request for obtaining focus. 
   * 
   * @param rowNumber this parameter is applicable only to array and arrayset
   *           controls; fields and other controls disregard this parameter
   */
  public void requestFocus(int rowNumber) {
    _comboBox.requestFocus();  
  }
    
  /**
   * Sets the selected index on this combo boc control. 
   * 
   * @param value the index of the item to be selected as a 
   *         <code>java.lang.String</code>
   */
  public void setValue(String value) {
    SET_VALUE_FLAG = true;
    _comboBox.setSelectedItem(value);
    SET_VALUE_FLAG = false;
  }
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _comboBox;
  }
   
  /**
   * Sets the items on this combo box control.
   * 
   * @param items the items to be set on this combo box control
   */
  public void setItems(String[] items) {
    _comboBox.setMaximumRowCount(15);
    SET_VALUE_FLAG = true;
    _comboBox.removeAllItems();
    for (int i=0; i<items.length; i++) 
    {
      _comboBox.addItem(items[i]);
    }
    SET_VALUE_FLAG = false;
  }
  
  /**
   * Disposes this component.
   */
  public void dispose() {
    _comboBox.removeFocusListener(_focusListener);
    _comboBox.removeItemListener(_itemListener);
    _comboBox.removeMouseListener(_mListener);
    super.dispose();
  }
    
  /**
   * Inner class that listens to loss and gain of focus on this control.
   */
  class FocusWatcher implements FocusListener {
    /**
     * This method is called when this control gains focus. 
     * 
     * @param e the event that is generated when this control  gains focus
     */
    public void focusGained(FocusEvent e) {
      /**
       * Implementation of red outline when the component has the focus
       */
       _comboBox.setBorder(BorderFactory.createLineBorder(Color.red, 2));
      
       GUIControlEvent event = new 
          GUIControlEvent(ComboBoxControl.this, 
            GUIControlEvent.COMPONENT_FOCUS_EVENT,
            null);
       try {
          fireGUIControlChanged(event);
       } 
       catch (GUIControlException en) {
          System.err.println("Combo Box Focus Exception");  
       }
    }

    /**
     * This method is called when this control loses focus. 
     * 
     * @param e the event that is generated when this control loses focus
     */        
    public void focusLost(FocusEvent e) {
     /**
      * Return border to default
      */
      _comboBox.setBorder(BorderFactory.createEtchedBorder());
    }
  }
  
  /**
   * Inner class to listen to item selections on this combo box control.
   */
  class SelectionListener implements ItemListener {
    
    /**
     * This method is invoked when selection on the combo box control changes.
     * 
     * @param e the event that is generated when the item selection changes 
     */
    public void itemStateChanged(ItemEvent e) {
      if (SET_VALUE_FLAG) {
        return;
      }
      if ( e.getStateChange() == ItemEvent.DESELECTED ) {
        return;
      }
      
      GUIControlEvent event = new 
        GUIControlEvent(ComboBoxControl.this, 
          GUIControlEvent.MODIFY_FIELD_EVENT,
          _comboBox.getSelectedItem());
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.err.println("ComboBox itemStateChanged Exception");  
      }
    }
  }

  /**
   * Workaround for the focus problem with combo boxes.
    */
  class MyComboBoxUI extends BasicComboBoxUI {
     protected void installListeners() {
       super.installListeners();
       comboBox.addFocusListener(new FocusWatcher());  
     }
  }
  
  /**
   * Inner class that implements a mouse listener to listen 
   * to mouse entered events on this control.
   */      
  class MyMouseListener extends MouseAdapter {
    /**
     * Method that is invoked when the combo box  detects a mouse click.
     * add interaction for bug #103
     */
    public void mouseClicked(MouseEvent e){
      if(!_isSensitive)  {
        GUIControlEvent event = new 
          GUIControlEvent(ComboBoxControl.this, 
            GUIControlEvent.ITEM_CLICKED_EVENT,
              "");
        try
        {
          fireGUIControlChanged(event);
        }
        catch (GUIControlException en) {
          System.err.println("ComboBoxControl mouseClicked: Exception");  
        }
      }
    }

    /**
     * Method that is invoked when the button detects a mouse enter.
     */
    public void mouseEntered(MouseEvent e) {
      GUIControlEvent event = new 
        GUIControlEvent(ComboBoxControl.this, 
          GUIControlEvent.MOUSE_ENTERED_EVENT,
            null);
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.err.println("ComboBoxControl: Exception");  
      }
    }  
  }
}
