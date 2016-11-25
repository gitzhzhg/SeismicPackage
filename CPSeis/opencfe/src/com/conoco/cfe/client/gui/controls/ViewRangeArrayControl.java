// ViewRangeArrayControl.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.utils.ArrayList;

import java.awt.Point;
import java.awt.Component;
import java.awt.Font;

import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseAdapter;

import java.util.Vector;
import java.util.Hashtable;

import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.DefaultListSelectionModel;
import javax.swing.ListSelectionModel;
import javax.swing.DefaultListModel;

import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;

/**
 * A GUI control that encapsulates the functionality of a list
 * component.
 */
public class ViewRangeArrayControl 
  extends GUIControlAdapter implements ArrayGUIControl {
  /**
   * Variable for the list component to be contained 
   * by this control
   * 
   * @serial
   */
  protected JList _list;
  
  /**
   * Variable for the scroll pane that is used to contain
   * the list component
   * 
   * @serial
   */
  protected JScrollPane _scroll;
  
  /**
   * Variable for the focus listener
   * 
   * @serial
   */
  protected FocusListener _focusListener;
  
  /**
   * Variable for the list selection listener
   * 
   * @serial
   */
  protected ListSelectionListener _selectionListener;
  
  /**
   * Variable for the mouse listener
   * 
   * @serial
   */
  protected MouseAdapter _mouseListener;
  
  /**
   */
  protected String _name;
    
  /**
   * Constructs a new array control.
   */      
  public ViewRangeArrayControl(String name) {
    super();
    _name = name;
    _list = new JList();
    _scroll = new JScrollPane(_list);
    _focusListener = new FocusWatcher();
    _selectionListener = new SelectionListener();
    _mouseListener = new ListMouseListener();
    _list.addFocusListener(_focusListener);
    _list.addListSelectionListener(_selectionListener);
    _list.addMouseListener(_mouseListener);
  }
  
  /**
   * Sets the items on this list component.
   * 
   * @param values the items to be set on this list component
   */
  public void setItems(String[] values) {
    _list.setListData(values);
  }
  
  /**
   * Sets the sensitivity of this control.
   * 
   * @param sensitive the boolean flag that is true if 
   *           this control is to be sensitive; false otherwise
   */
  public void setSensitive(boolean sensitive) {
    _list.setEnabled(sensitive);
  }
    
  /**
   * Posts a request for obtaining focus. 
   * 
   * @param rowNumber this parameter is applicable only to array and arrayset
   *           controls; fields and other controls disregard this parameter
   */
  public void requestFocus(int rowNumber) {
    DefaultListSelectionModel model = (DefaultListSelectionModel) 
    _list.getSelectionModel();
    model.setSelectionInterval(rowNumber, rowNumber);
    _list.requestFocus();
  }
  
  /**
   * Returns the number of rows in the array control.
   * 
   * @return the number of items in the array control
   */
  public int getRowCount() {
    return _list.getModel().getSize();
  }
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _scroll;
  }
  
  /**
   * Sets the font on this control.
   * 
   * @param f the desired font
   */
  public void setFont(Font f) {
    _list.setFont(f);  
  }
    
  public void modifyElements(String[] elements, int start, int end) {
  }
  
  public void insertElements(String[] elements, int start) {
  }
  
  public void fillArrayCombo(String[] elements){
  }
  
  public void deleteElements(int start, int end) {
  }
  
  public void clearSelection() {
  }  

  /**
   * Returns the elements contained by this array component.
   * 
   * @return   the elements contained by this array component
   *       as an array of <code>java.lang.String</code>
   */
  public String[] getElements() {
    return (String[]) ((DefaultListModel) _list.getModel()).toArray();
  }

  /**
   * Returns the index of the array element that has been 
   * modified.
   * 
   * @return the index of the element that has been modified
   */
  public int getChangedIndex() {
    return -1;
  }
  
  /**
   * Returns the column header.
   * 
   * @return the column header string
   */
  public String getColumnName() {
    return _name;  
  }
  
  /**
   * Inner class that sends focus-related notifications to the
   * GUI control listeners.
   */
  class FocusWatcher implements FocusListener {
    /**
     * This method is invoked when focus is gained by this 
     * control
     * 
     * @param e the event that is generated when focus
     *       is gained by this control
     */
    public void focusGained(FocusEvent e) {
    }
    
    /**
     * This method is invoked when focus is lost by this 
     * control
     * 
     * @param e the event that is generated when focus
     *       is lost by this control
     */    
    public void focusLost(FocusEvent e) {
      GUIControlEvent event = new 
        GUIControlEvent(ViewRangeArrayControl.this, 
          GUIControlEvent.LEAVE_ARRAY_EVENT,
            null);
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.err.println("ViewRange Array Focus Exception");  
      }
    }
  }  
  
  /**
   * Inner class that sends selection-related notifications to 
   * GUI control listeners.
   */
  class SelectionListener implements ListSelectionListener {
    /**
     * This method is invoked when a list selection event takes place
     * 
     * @param e the event that is generated when a list selection 
     *       changes
     */
    public void valueChanged(ListSelectionEvent e) {
      ListSelectionModel model = _list.getSelectionModel();
      if ( model.getValueIsAdjusting() ) {
        return;
      }
      int[] vals = _list.getSelectedIndices();
      String[] value = new String[vals.length];  
      for ( int i = 0; i < vals.length; i++) {
        value[i] = String.valueOf(vals[i]+1);
      }    
      
      GUIControlEvent event = new 
        GUIControlEvent(ViewRangeArrayControl.this, 
          GUIControlEvent.ITEMS_SELECTED_EVENT, 
            value);
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.err.println("ViewRange Array List Selection Exception");  
      }
    }
  }
  
  /**
   * Inner class that is used to send selection-related notifications
   * to GUI control listeners.
   */
  class ListMouseListener extends MouseAdapter {
    /**
     * This method is invoked when mouse is clicked on
     * this control
     * 
     * @param e the event that is generated when mouse
     *       is clicked on the list component
     */
    public void mouseClicked(MouseEvent e) {
      if (javax.swing.SwingUtilities.isLeftMouseButton(e)) {
        if ( e.getClickCount() >= 2 ) {
          ListSelectionModel model = _list.getSelectionModel();
          Point p = new Point(e.getX(), e.getY());

          GUIControlEvent event = new 
            GUIControlEvent(ViewRangeArrayControl.this, 
              GUIControlEvent.DOUBLE_CLICK_SELECTION_EVENT,
                String.valueOf(_list.locationToIndex(p)));
          try {
            fireGUIControlChanged(event);
          } 
          catch (GUIControlException en) {
            System.err.println("ViewRange Array Mouse Exception");  
          }
        }
      }
    }    
  }  
}