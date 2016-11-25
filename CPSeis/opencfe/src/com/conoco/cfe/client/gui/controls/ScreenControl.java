// ScreenControl.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.table.ArrayComponent;
import com.conoco.cfe.client.gui.controls.table.RowTable;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Font;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.util.Vector;
import java.util.Enumeration;

import javax.swing.JPanel;
import javax.swing.JScrollPane;

/**
 * A GUI control that encapsulates a screen component.
 */
public class ScreenControl extends GUIControlAdapter {
  /**
   * Variable to hold the screen component
   * 
   * @serial
   */
  protected Screen _screen;
  
  /**
   * Variable to hold the screen title
   * 
   * @serial
   */
  protected String _title;
  
  /**
   * Declares a variable for the popup menu.
   * 
   * @serial
   */
  protected PopupMenuControl _popup;
  
  /**
   * Declares a variable for the mouse listener object.
   * 
   * @serial
   */
  protected MouseAdapter _clickListener;
  
  /**
   * Declares a variable for the controls that are added 
   * to the screen control
   * 
   * @serial
   */
  protected Vector _controls;

  /**
    * Boolean variable that is set if this screen is scrollable.
   * 
   * @serial
   */
  protected boolean _doesScroll = false;  

  /**
    * Variable for scroll pane
   * 
   * @serial
   */
  protected JScrollPane _scroll;
  
  /**
   * Variable for the screen viewport adjuster
   *
   * @serial
   */
  protected ScreenViewportAdjuster _adjuster;

  /**
   * Constructs a new screen control
   * 
   * @param columns the number of columns that this screen should have
   * @param rows the number of rows this screen should have
   * @param minCellWidth the minimum width a cell should have
   * @param minCellHeight the minimum height a cell should have
   */
  public ScreenControl(int columns, int rows, int minCellWidth, int minCellHeight) {
    super();
    _screen = new Screen();
    _controls = new Vector();
    _clickListener = new ClickListener();
    _screen.setLayout( new CellLayout(columns, rows, 
                                        minCellWidth, minCellHeight));
    _screen.addMouseListener(_clickListener);
    _scroll = new JScrollPane(_screen);
    _scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);      
    _scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
  }
        
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    if ( isScrollable() ) {
      return _scroll;
    } 
    else {
      return _screen;
    }
  }  
  
  /**
    * Identifies whether this screen is scrollable or not.
   * 
   * @return   a boolean variable that is set to true if this screen 
   *     is scrollable
    */
  public boolean isScrollable() {
    return _doesScroll;
  }  

  /**
   * Sets whether this screen should be scrollable or not.
   * 
   * @param scrollable   is true if this screen is to be scrollable;
   *       false otherwise
    */
  public void setScrollable(boolean scrollable) {
    _doesScroll = scrollable;
    if ( _doesScroll ) {
      _adjuster = new ScreenViewportAdjuster(ScreenControl.this);  
    }
  }

  /** 
   * Sets the title of this screen control.
   * 
   * @param s the desired title of this screen control
   */
  public void setTitle(String s) {
    _title = s;
  }
  
  /**
   * Returns the title of this control
   * 
   * @return the title of this control
   */
  public String getTitle() {
    return _title;
  }
  
  /**
   * Add a GUI control to this screen control.
   * 
   * @param mic the GUI control to be added to this control
   */
  public void add(GUIControl mic) {
    _controls.addElement(mic);
    if ( _adjuster != null ) {
      mic.addGUIControlListener(_adjuster);
    }
    add(mic.getComponent());
  }
  
  /**
   * Add a component to this screen control.
   * 
   * @param c the component to be added to this control
   */
  protected void add(Component comp) {
    _screen.add(comp);
  }
    
  /**
   * Sets the constraints on a GUI control.
   *
   * @param control   the control on which the constraints are to be
   *           set
   * @param constraint  the constraints to be set on the control
   */
  public void setConstraint(GUIControl control, Object constraint) {
    setConstraint(control.getComponent(), constraint);
  }
            
  /**
   * Sets the constraints on a component.
   *
   * @param control   the component on which the constraints are to be
   *           set
   * @param constraint  the constraints to be set on the component
   */
  public void setConstraint(Component c, Object cons) {
    ((CellLayout) _screen.getLayout()).addLayoutComponent(c, cons);
  }
  
  /**
   * Activates the popup menu on this screen.
   * 
   * @param pc the popup to be attached
   */
  public void activatePopupMenu(PopupMenuControl pc) {
    _popup = pc;    
  }
  
  /**
   * Disposes this control.
   */
  public void dispose() {
    _screen.removeMouseListener(_clickListener);
    Enumeration en = _controls.elements();
    while ( en.hasMoreElements() ) {
      Object o = en.nextElement();
      ((GUIControl) o).dispose();
    }
    _controls.removeAllElements();
  }
  
  /**
   * Inner class to implement the functionality of a screen object.
   * The screen uses a custom layout manager for laying out
   * its components. The screen also maked sure that border panels
   * are added so that they are behind all the other components.
   * 
   * @see com.conoco.cfe.client.gui.controls.CellLayoutManager
   */
  class Screen extends JPanel {
    /**
     * Constructs a new screen object.
     */
    public Screen() {
      super();
    }
    
    /**
     * Adds a specified component with the specified constraints.
     * 
     * @param comp the component to be added
     * @param constraints   the constraints object using which the 
     *             component is to be added
     * @param index      the position at which the component  is to be added
     */
    protected void addImpl(Component comp, Object constraints, int index)     {
      if ( comp instanceof JPanel ) {
        super.addImpl(comp, constraints, getComponents().length-1);
      }
      else {
        super.addImpl(comp, constraints, index);
      }                     
    }
  }
  
  /**
   * Inner class that listens to mouse clicks on the screen
   * to bring up the popup menu.
   */
  private class ClickListener extends MouseAdapter {
    public void mouseClicked(MouseEvent e) {  
      if ( javax.swing.SwingUtilities.isRightMouseButton(e) ) {
        if ( _popup != null ) {
          _popup.show( e.getComponent(), e.getX(), e.getY());
        }
      }
    }
  }
}