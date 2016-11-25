///
/// TabPaneControl.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 08-13-2002 SMCook   Added call to setForeground() in setSelectedIndex()
///                          to make sure selected tab label is red on startup.
///

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Console;

import  com.conoco.cfe.client.gui.controls.ui.ModifiedTabbedPaneUI;

import com.conoco.cfe.utils.ArrayList;

import java.awt.Component;
import java.awt.Font;
import java.awt.Color;

import java.awt.event.MouseEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;

import java.util.Hashtable;
import java.util.Enumeration;

import javax.swing.JTabbedPane;
import javax.swing.BorderFactory;
import javax.swing.DefaultSingleSelectionModel;
import javax.swing.UIManager;

/**
 * A GUI control that encapsulates a tab pane. 
 */
public class TabPaneControl extends GUIControlAdapter {
  /**
   * The tabbed pane component contained by this control
   * 
   * @serial
   */
  protected JTabbedPane _tabPane;
  
  /**
   * Variable for storing the index of the selected tab
   * 
   * @serial
   */
  protected int _selectedIndex;
  
  /**
   * Lookup table that maps a given index to the screen 
   * control at that index
   * 
   * @serial
   */
  protected Hashtable _indexScreenControlLookup;
  
  /**
   * Variable for storing the current index value. The
   * current index is incremented by one each time 
   * a tab is added
   * 
   * @serial
   */
  protected int _currentIndex;

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
   * Constructs a tabbed pane control
   */    
  public TabPaneControl() {
    super();
    _tabPane = new JTabbedPane();
    _tabPane.setUI(new ModifiedTabbedPaneUI());
    _indexScreenControlLookup = new Hashtable();
    _tabPane.setModel(new TabSelectionModel());
    _mListener = new MyMouseListener();
    _focusListener = new FocusWatcher();
    _tabPane.addMouseListener(_mListener);
    _tabPane.addFocusListener(_focusListener);
  }
  
  /**
   * Adds a screen control to the tabbed pane component.
   * 
   * @param c the screen control to be added
   */
  public void add(ScreenControl c) {
    getIndexScreenKeyLookup().put( new Integer(_currentIndex), c);
    ++_currentIndex;
    _tabPane.add(c.getTitle(), c.getComponent());
  }

  /**
   * Sets the font on this control.
   * 
   * @param f the new font as <code>java.awt.Font</code>
   */
  public void setFont(Font f) {
    _tabPane.setFont(f);
  }
  
  /**
   * Sets the sensitivity of this control.
   * 
   * @param sensitive the boolean flag that is true if 
   *           this control is to be sensitive; false otherwise
   */
  public void setSensitive(int index, boolean sensitive) {
    _tabPane.setEnabledAt(index, sensitive);
  }
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _tabPane;
  }
  
  /**
   * Posts a request for obtaining focus. 
   * 
   * @param rowNumber this parameter is applicable only to array and arrayset
   *           controls; fields and other controls disregard this parameter
   *           for array and araysets, it is the number of the row that is
   *           to gain focus
   */
  public void requestFocus(int tabNumber) {
    _tabPane.setSelectedIndex(tabNumber);
  }
  
  /**
   * Returns the keyword of the screen control that was last 
   * selected. This method is typically called by a listener
   * on the tab to access the screen control at the index
   * that was previously selected.
   * 
   * @return the keyword of the previously selected screen control
   */
  public String getKeywordOfLastScreen() {
    return ((GUIControl) getIndexScreenKeyLookup().get( 
                new Integer(_selectedIndex) )).getKeyword();
  }
  
  /** 
   * Returns the lookup table that maps screen index to the 
   * screen key.
   * 
   * @return the lookup table
   */
  public Hashtable getIndexScreenKeyLookup() {
    return _indexScreenControlLookup;
  }
  
  /**
   * Returns the index of the screen that ha the specified keyword.
   * 
   * @param screenKeyword the keyword of the screen whose 
   *             index is desired
   */
  public int getIndexOfScreen(String screenKeyword) {
    Enumeration keys = getIndexScreenKeyLookup().keys();
    while (keys.hasMoreElements()) {
      Object o = keys.nextElement();
      ScreenControl sc = (ScreenControl) getIndexScreenKeyLookup().get(o);      
      if (screenKeyword.equals(sc.getKeyword().toUpperCase())) {
        return ((Integer) o).intValue();    
      }
    }  
    return -1;  
  }
    
  /**
   * Returns the number of tabs in this control.
   * 
   * @return the number of tabs
   */
  public int getTabCount() {
    return _tabPane.getTabCount();
  }
  
  /**
   * Returns the component at the specified index in this
   * tab pane control.
   *
   * @return the component at the specified index
   */
  public Component getComponentAt(int i) {
    return _tabPane.getComponentAt(i);
  }
  
  /**
   * Disposes this control.
   */
  public void dispose() {
    Enumeration keys = getIndexScreenKeyLookup().keys();
    while (keys.hasMoreElements()) {
      Object o = keys.nextElement();
      ((ScreenControl) getIndexScreenKeyLookup().get(o)).dispose();
    }
    getIndexScreenKeyLookup().clear();
    _tabPane.removeMouseListener(_mListener);
    _tabPane.removeFocusListener(_focusListener);  
  }
  
  /**
   * Inner class that listens to tab selections ( screen selection ). 
   */
  class TabSelectionModel extends DefaultSingleSelectionModel {  
    /**
     * This method is invoked when a tab pane ( screen ) is 
     * selected. 
     * 
     * @int index the index of the tab pane selected
     */
    public void setSelectedIndex(int index) {
      GUIControlEvent event = new 
        GUIControlEvent(TabPaneControl.this, 
          GUIControlEvent.LEAVE_SCREEN_EVENT,
            new Integer(_selectedIndex));
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        return;
      }
      event = new 
        GUIControlEvent(TabPaneControl.this, 
          GUIControlEvent.ENTER_SCREEN_EVENT,
            new Integer(index));
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        return;
      }
      _selectedIndex = index;
      super.setSelectedIndex(index);

      for (int i=0; i<_tabPane.getTabCount(); i++) {  //SMCook
          _tabPane.setForegroundAt(i, Color.black);   // assures red color at
      }                                               // initial startup
      _tabPane.setForegroundAt(index, Color.red);     //
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
      /**
       * Implementation of red outline when the component has the focus
       */
       _tabPane.setForegroundAt(_tabPane.getSelectedIndex(), Color.red);      
    }

    /**
     * This method is called when this control loses focus. 
     * 
     * @param e the event that is generated when this control
     *       loses focus
     */        
    public void focusLost(FocusEvent e) {
      /**
      * Return border to default
      */
      _tabPane.setForegroundAt(_tabPane.getSelectedIndex(), Color.black);      
    }
  }

  /**
   * Inner class that implements a mouse listener to listen 
   * to mouse entered events on the tabPane.
   */      
  class MyMouseListener extends MouseAdapter {
    /**
     * Method that is invoked when the tabPane detects a mouse enter.
     */
    public void mouseClicked(MouseEvent e) {
     /**
       * Implementation of red outline when the component has the focus
       */
      for (int i=0; i<_tabPane.getTabCount(); i++) {
          _tabPane.setForegroundAt(i, Color.black);
      }
       _tabPane.setForegroundAt(_tabPane.getSelectedIndex(), Color.red);  
    }
  }      
}
