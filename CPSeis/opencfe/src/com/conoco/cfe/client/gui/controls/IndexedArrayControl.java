///
/// IndexedArrayControl.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  6.
///  5. 09-05-2002 SMCook   ColumnData objects are now created with click count
///                          to edit as 1 or 2, depending on the desired
///                          behavior of the particular array.
///                         Removed inner class MyScrollPane.
///                         Cleaned up line lengths and tabs.
///  4. 08-02-2002 SMCook   Removed overrided paint() function.
///  3. 09-25-2001 SMCook   Removed debug print statement.
///  2. 09-24-2001 SMCook   Testing changes w/MyScrollPane inner class.
///  1. 09-18-2001 SMCook   New class incorporating previous functionality of
///                          IndexedComboArrayControl, 
///                          IndexedFloatArrayControl, 
///                          IndexedIntegerArrayControl, and 
///                          IndexedStringArrayControl. 
///

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.table.ArrayComponent;
import com.conoco.cfe.client.gui.controls.table.ColumnData;
import com.conoco.cfe.client.gui.controls.table.IntegerColumnData;
import com.conoco.cfe.client.gui.controls.table.FloatColumnData;
import com.conoco.cfe.client.gui.controls.table.RowTable;

import com.conoco.cfe.utils.EventQueue;

import java.awt.Component;
import java.awt.Container;
import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Dimension;

import java.awt.event.MouseEvent;
import java.awt.event.MouseAdapter;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.BorderFactory;

import javax.swing.border.Border;

/**
 * An indexed array component that accepts integer values.
 * 
 */
public class IndexedArrayControl extends ArrayGUIControlAdapter {
  /**
   * Variable to hold the panel component
   * 
   * @serial
   */
  protected JPanel _panel;

  /**  
   * Variable for the scroll pane that constains the table
   * component of this control
   * 
   * @serial
   */
  protected JScrollPane _scroll;

  /**
   * Variable for the row table component that will show 
   * the table headers
   * 
   * @serial
   */
  protected RowTable _rowTable;

  /**
   * Variable for the mouse listener 
   * 
   * @serial
   */
  protected MouseAdapter _mListener;
 
  /**
   * Variable for the default scroll pane border
   * 
   * @serial
   */
  protected Border _defBorder;
 
  /**
   * Constructor 1 of 1.
   * 
   * @param name the label to be used for this control
   */      
  public IndexedArrayControl(String name, String type) {
    super(name);

//create ColumnData according to data type
    ColumnData columnData=null;

    if     (type.equals("int")) {
      columnData = new IntegerColumnData(1, _array);
    }
    else if(type.equals("float")) {
      columnData = new FloatColumnData(1, _array);
    }
    else if(type.equals("string")) {
      if     (name.equals("Old Process")) {
        columnData = new ColumnData(2, _array);
      }
      else if(name.equals("Process List")) {
        columnData = new ColumnData(2, _array);
      }
      else if(name.equals("Process")) {
        columnData = new ColumnData(2, _array);
      }
      else if(name.equals("DIRECTORIES")) {
        columnData = new ColumnData(2, _array);
      }
      else {
        columnData = new ColumnData(1, _array);
      }
    }
    else if(type.equals("combo")) {
      columnData = new ColumnData("combo");
    }
    else {
      throw new IllegalArgumentException("IndexedArrayControl: illegal type");
    }

    columnData.setColumnName(name);

    _array.addColumnData(columnData);

//continue with construction
    _scroll = new JScrollPane(_array);
    _array.setSizeLimiter(_scroll.getViewport());       //SMCook added

    _scroll.setHorizontalScrollBarPolicy(
      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    _scroll.setVerticalScrollBarPolicy(
      JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);

    _defBorder = _scroll.getBorder();
    _mListener = new MyMouseListener();
    _scroll.addMouseListener(_mListener);
    _panel = new JPanel(new BorderLayout(), true);  //SMCook double-buffer

    _panel.getInsets().top = 0;
    _panel.getInsets().bottom = 0;
    _panel.getInsets().left = 0;
    _panel.getInsets().right = 0;

    _rowTable = new RowTable(_array.getModel());
    _scroll.getViewport().addChangeListener(_rowTable);
    _panel.add(_rowTable, BorderLayout.CENTER);
    _scroll.setRowHeaderView(_panel);  
    addGUIControlListener(new InnerListener());
  }
 
  /**
   * Sets the font on this control.
   * 
   * @param f the desired font
   */
  public void setFont(Font f) {
    super.setFont(f);
    _rowTable.setCurrentFont(f);
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
   * Inner class that listens to mouse clicks on the scroll pane.
   */
  protected class MyMouseListener extends MouseAdapter {
    public void mousePressed(MouseEvent e) {  
      java.awt.Rectangle r = _array.getCellRect(0,0,true);
      _scroll.setBorder(BorderFactory.createLineBorder(Color.red, 2));
      _array.requestFocus();            
    }

    public void mouseEntered(MouseEvent e) {
      GUIControlEvent event = new 
        GUIControlEvent(IndexedArrayControl.this, 
          GUIControlEvent.MOUSE_ENTERED_EVENT,
            null);
      try {
        fireGUIControlChanged(event);
      }
      catch (GUIControlException ex) {
      }
    }
  }
 
  /**
   * Inner class that listens to focus lost event on this array component.
   */      
  protected class InnerListener implements GUIControlListener {
    public void guiControlChanged(GUIControlEvent e) {  
      if (e.getType() == GUIControlEvent.LEAVE_ARRAY_EVENT) {
        _scroll.setBorder(_defBorder);  
      }
    }
  }

}
