///
/// ArraySetControl.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
/// 10.
///  9. 09-27-2002 SMCook   Added protective code to prevent MODIFY event from 
///                          being sent under certain conditions.
///  8. 09-05-2002 SMCook   Now inherits from new KeyListenerJTable class.
///                         Moved responsibility for calling stopCellEditing()
///                          to the KeyListenerJTable class.
///                         Cleaned up line length and tabs.
///                         Removed inner class MyScrollPane.
///  7. 10-04-2001 SMCook   Removed 3 unused functions -- relict code that was
///                          causing useless hits when grepping and deciphering
///                          the code.
///  6. 09-29-2001 SMCook   Added call to _array.stopCellEditing() if focus
///                          is lost and mouse is not over the ArrayComponent
///                          (part of fix for bug report 514, item 1).  This
///                          required adding a new variable _mouseIsOutside.
///  5. 09-25-2001 SMCook   Removed debug print statement.
///  4. 09-24-2001 SMCook   Testing changes w/MyScrollPane inner class.
///  3. 09-18-2001 SMCook   Added call to _array.setSizeLimiter() in order to
///                          give the _array (JTable) knowledge of the size of
///                          the container it's in.  Needed as part of fix for
///                          horizontal scroll bar annoyance.
///

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.table.KeyListenerJTextField;

import com.conoco.cfe.client.gui.controls.table.IntegerColumnData;
import com.conoco.cfe.client.gui.controls.table.FloatColumnData;
import com.conoco.cfe.client.gui.controls.table.ColumnData;
import com.conoco.cfe.client.gui.controls.table.ColumnTableModel;
import com.conoco.cfe.client.gui.controls.table.RowTable;
import com.conoco.cfe.client.gui.controls.table.KeyListenerJTable;

import com.conoco.cfe.utils.ArrayList;

import java.awt.Font;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Dimension;

import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionAdapter;

import java.util.Hashtable;
import java.util.Enumeration;

import javax.swing.SwingUtilities;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JComponent;
import javax.swing.JComboBox;
import javax.swing.BorderFactory;
import javax.swing.ListSelectionModel;
import javax.swing.table.TableModel;
import javax.swing.DefaultCellEditor;

import javax.swing.border.TitledBorder;
import javax.swing.border.Border;
  
import javax.swing.event.TableColumnModelListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelListener;
import javax.swing.event.TableModelEvent;

import javax.swing.table.TableColumnModel;
import javax.swing.table.TableColumn;

/** 
 * A GUI control for an arrayset. An arrayset is a container 
 * of array components. 
 */
public class ArraySetControl extends ArrayGUIControlAdapter
  implements ArraySetGUIControl {

  /**
   * Variable to store the index of the column added to this control
   *  
   * @serial
   */
  protected int _columnIndex = 0;
 
  /**
   * Lookup for accessing a column keyword using its index number
   * 
   * @serial
   */
  protected Hashtable _indexKeywordLookup;

  /**
   * Variable for storing the column index of the cell that
   * is selected
   * 
   * @serial
   */
  protected int _selectedColumn = -1;

  /**
   * Lookup for accessing the column wrapper object using its keyword
   * 
   * @serial
   */
  protected Hashtable _keywordWrapperLookup;      

  /**
   * Variable to hold the panel component
   * 
   * @serial
   */
  protected JPanel _panel;

  /**
   * Variable to keep track of mouse
   *
   * @serial
   */
  protected boolean _mouseIsOutside;

  /**    
   * The scroll pane that will contain this array component.
   * 
   * @serial
   */
  protected JScrollPane _scroll;
 
  /**
   * Declares variable for the row table component that shows the row header
   * 
   * @serial
   */
  protected RowTable _rowTable;
 
  /**
   * Declares the variable for the arrayset label
   * 
   * @serial
   */
  protected String _label;
 
  /**
   * Declares the variable for the panel component that 
   * contains the scroll pane containing the table and 
   * which has a titled border with the arrayset label
   * 
   * @serial
   */
  protected JPanel _container;
 
  /**
   * Variable for the listener object that sends notification 
   * to the GUI listeners when the model of this control changes
   * or when this control goes out of focus
   * 
   * @serial
   */
  protected MySelectionListener _hybridListener;
 
  /**
   * Variable for the columns which are added to this control
   * 
   * @serial
   */
  protected ArrayList _columns;

  /**
    * Variable for the default scroll pane border
    * 
    * @serial
    */
  protected Border _defBorder;

  /**
   * Constructs a new arrayset control.
   */      
  public ArraySetControl() {
    super(); 
    _array.setRowSelectionAllowed(true);
    _indexKeywordLookup = new Hashtable();
    _keywordWrapperLookup = new Hashtable();

    _scroll = new JScrollPane(_array);
    _array.setSizeLimiter(_scroll.getViewport());     //SMCook

    _defBorder = _scroll.getBorder();

    _scroll.addMouseListener(new MyScrollMouseListener());
    _scroll.addMouseMotionListener(new MyScrollMouseMotionListener());

    _scroll.setHorizontalScrollBarPolicy(
      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    _scroll.setVerticalScrollBarPolicy(
      JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);

    _panel = new JPanel(new BorderLayout(), true);      //SMCook double-buffer
    _panel.getInsets().top = 0;
    _panel.getInsets().bottom = 0;
    _panel.getInsets().left = 0;
    _panel.getInsets().right = 0;    
    _rowTable = new RowTable(_array.getModel());
    _panel.add(_rowTable, BorderLayout.CENTER);

    _scroll.setRowHeaderView(_panel);  
    _scroll.getViewport().addChangeListener(_rowTable);

    _container = new JPanel(new BorderLayout(), true);  //SMCook double-buffer
    _container.add(_scroll);

    _columns = new ArrayList();
    addGUIControlListener(new InnerListener());
  }
 
  /**
   * Constructs a new arrayset control with specified label.
   * 
   * @param label the label of the array set
   */      
  public ArraySetControl(String label) {
    this();
    _label = label;
    if (_label != null) {
      _container.setBorder( BorderFactory.createTitledBorder(_label) );
    } 
    else {
      if (_label.length() != 0) {
        _container.setBorder(BorderFactory.createTitledBorder(getKeyword()));
      }
    }  
  }
 
  /**
   * Adds a column to this arrayset control using the attributes specified.
   *
   * @param keyword   the keyword of the array component which is to be
   *           added to this arrayset
   * @param type    the type of the array control
   * @param maxLength  the maximum number of characters that can be inserted
   *           in a editor component of the array component
   * @param columnHeader  the table header that will be used for this array
   *             component
   * @param editable  a boolean variable that indicates whether the array
   *           component wil be editable or not
   * @param sensitive  a boolean variable that indicates whether the array
   *           component wil be sensitive or not
   * @return      the array component that is added to this arrayset
   */
  public ArrayGUIControl addColumn(String keyword, String type, int maxLength,
    int columnSize, boolean sizeMaxLength, String columnHeader,
    boolean editable, boolean sensitive) {

    ColumnData columnData = null;

    if (type.equals("int")) {
      columnData = new IntegerColumnData(1, _array);
    }
    else if (type.equals("float")) {  
      columnData = new   FloatColumnData(1, _array);
    }
    else if (type.equals("string")) {
      columnData = new        ColumnData(1, _array);
    }
    else if (type.equals("combo")) {
      columnData = new ColumnData(keyword);
    }
    else {
      System.err.println("ArraySetControl: unknown type = " + type);
    }

    columnData.setColumnName(columnHeader);
    columnData.setMaxLength(maxLength);
    columnData.setColumnSize(columnSize, sizeMaxLength);
    columnData.setEditable(editable);

    _array.addColumnData(columnData);

    _indexKeywordLookup.put(new Integer(_columnIndex), keyword);
    ++_columnIndex;

    ColumnWrapper wrapper =  new ColumnWrapper();
    wrapper.setKeyword(keyword);
 
    _keywordWrapperLookup.put(keyword, wrapper);

    _columns.add(wrapper);
    return wrapper;
  }

  /**
   * SMCook added.
   */
  public boolean isManagingFocus() {
    return false;
  }

  /**
   * Returns the array of array components that have been added to 
   * this arrayset.
   * 
   * @return   the array of array components that have been added to this
   *       arrayset
   */
  public ArrayGUIControl [] getColumns() {
    ArrayGUIControl [] columns = new ArrayGUIControl[_columns.size()];
    for (int i = 0; i <_columns.size(); i++) {
      columns[i] = (ArrayGUIControl) _columns.get(i);
    }
    return columns;
  }

  /**
   * SMCook footnote - this clearSelection function (which is not something
   *  that I added) overrides clearSelection in ArrayGUIControlAdapter
   */
  public void clearSelection(){
  }

  /**
   * Returns the keyword of the array which has been modified
   * 
   * @return the keyword of the array that has been modified
   */
  public String getKeywordOfArrayChanged() {
    return (String) _indexKeywordLookup.get(new Integer(getChangedColumn())); 
  }
 
  /**
   * Returns the panel component that contains the scroll pane containing
   * the table and which has a titled border with the arrayset label.
   * 
   * @return the GUI component contained by this arrayset control
   */
  public Component getComponent() {
    return _container;
  }
 
  /**
   * Sets he font on this control.
   * 
   * @param f the desired font
   */
  public void setFont(Font f) {
    super.setFont(f);    
    if ( _rowTable != null ) {
      _rowTable.setCurrentFont(f);
    }
    if (_container != null && _container.getBorder() != null) {
      ((TitledBorder) _container.getBorder()).setTitleFont(f);
    }
  }
 
  /**
   * Disposes this control.
   */
  public void dispose() {
    if ( _hybridListener != null ) {
      _array.getColumnModel().removeColumnModelListener(
        _hybridListener);
    }
    _scroll.remove(_array);
    _panel.removeAll();
    _container.removeAll();
    super.dispose();
  }
 
  /**
   * Adds the list selection listener on the array component.
   */
  protected void addListSelectionListener() {
    _hybridListener = new MySelectionListener();
    _array.getSelectionModel().addListSelectionListener(_hybridListener);
    _array.getColumnModel().addColumnModelListener(_hybridListener);
  }

  /**
   * Adds the model listener on the array component.
   */
  protected void addModelListener() {
    _tableModelListener = new MyTableModelListener();
    _array.getModel().addTableModelListener(_tableModelListener);
  }
 
  /**
   * Adds a mouse listener on the array for double clicks
   */
  protected void addMouseListener() {
    _mouseListener = new MouseHandler();
    _array.addMouseListener(_mouseListener);
  }

  /**
   * Adds the focus listener.
   */
  protected void addFocusListener() {
    _array.addFocusListener(new FocusWatcher());
  }

  /**
   * Class used to mimic arrays contained by this control.
   */
  class ColumnWrapper implements ArrayGUIControl, GUIControl {
    /**
     * Variable for storing the keyword of this array
     */
    String _keyword;

    /**
     */
    int _localChangedIndex = -1;

    /**
     * Constructs a new array component
     */    
    public ColumnWrapper() {
    }
 
    /**
     * Returns the index of this array component. The index is found
     * using the keyword. 
     *
     * @return   the index of this array component; -1 if the 
     *       keyword of the array is not found
     */
    private int getIndex() {
      Enumeration keys = _indexKeywordLookup.keys();
      while (keys.hasMoreElements()) {
        Integer i = (Integer) keys.nextElement();
        String key = (String) _indexKeywordLookup.get(i);
        if ( key.equals(_keyword)) {
          return i.intValue();   
        }
      }  
      return -1;  
    }

    /**
     * Modifies the elements of this array component.
      * 
      * @param elements   the array of <code>java.lang.String</code>
      *          that describe the new data values
      * @param start the index of the first element to be modified
      * @param end the index of the last element to be modified
      */
    public void modifyElements(String[] elements, int start, int end) {      
      int colIndex = getIndex();  
      _array.setFlag(true);
      _array.clearSelection();  //SMCook
      ColumnTableModel model = (ColumnTableModel) _array.getModel();
      for ( int i = start; i <= end; i++) {
        model.setValueAt(elements[i], i, colIndex);  
      }  
      _array.setFlag(false);
    }

    /**
      * Inserts the elements in this array control.
      * 
      * @param elements   the array of <code>java.lang.String</code>
      *          that describe the new data values
      * @param start the index of the first element to be inserted
      */
    public void insertElements(String[] elements, int start) {
      int colIndex = getIndex();
      _array.setFlag(true);
      ColumnTableModel model = (ColumnTableModel) _array.getModel();
      for ( int i = 0; i < elements.length; i++) {
        model.insertRow(start+i, colIndex, elements[i]);
      }
      _array.setFlag(false);
    }

    /**
      * To fill the combo component in the array.
      * 
      * @param elements   values to set in the combo box component
      */
    public void fillArrayCombo(String[] elements){
      int colIndex = getIndex();
      TableColumn column = _array.getColumnModel().getColumn(colIndex);
      DefaultCellEditor cellEditor = (DefaultCellEditor) column.getCellEditor();
      Component cellEditorComponent = cellEditor.getComponent();

      if (cellEditorComponent instanceof JComboBox){
        JComboBox combo = new JComboBox ();
        combo = (JComboBox) cellEditorComponent;
        combo.removeAllItems();
        for ( int i = 0; i < elements.length; i++) {
          combo.addItem(elements[i]);
        }
      }
    }

    /**
      * Deletes the elements from this array control.
      * 
      * @param start the index of the first element to be deleted
      * @param end the index of the last element to be deleted
      */
    public void deleteElements(int start, int end) {
      int colIndex = getIndex();  
      _array.setFlag(true);
      ColumnTableModel model = (ColumnTableModel) _array.getModel();
      if ( model.getRowCount() != 0 ) {
        for ( int i = end; i >= start; i--) {
          model.removeRow(i, colIndex);  
        }
      }
      _array.setFlag(false);            
    }  

   /**
     * remove focus and border on cells selected.
     */
    public void clearSelection(){
      _array.clearSelection();
    }

   /**
     * Returns the elements contained by this array component.
     * 
     * @return   the elements contained by this array component
     *       as an array of <code>java.lang.String</code>
     */
    public String[] getElements() {
      ColumnTableModel model = (ColumnTableModel) _array.getModel();
      return model.getDataForIndex( getIndex() );
    }

    /**
     */
    public void enableKeyBinding(String key) {
    } 

   /**
     * Returns the number of rows in the array control.
     * 
     * @return the number of items in the array control
     */
    public int getRowCount() {
      ColumnTableModel model = (ColumnTableModel) _array.getModel();
      ColumnData data = model.getColumn(getIndex());
      return data.getRowCount();
    }

    /**
     * Returns the index of the array element that has been 
     * modified.
     * 
     * @return the index of the element that has been modified
     */
    public int getChangedIndex() {
      return _localChangedIndex;
    }

    /**
     * Returns the column header.
     * 
     * @return the column header string
     */
    public String getColumnName() {
      ColumnData column =  _array.getColumnData(getIndex());
      return column.getColumnName();  
    }

    public void addGUIControlListener(GUIControlListener l) {
    }

    public void removeGUIControlListener(GUIControlListener l) {
    }

    public void setFont(java.awt.Font f) {
    }

    public void setSensitive(boolean sensitive) {
      ColumnData column =  _array.getColumnData(getIndex());
      column.setEditable(sensitive);
    }

    public void setEditable(boolean editable) {
      ColumnData column =  _array.getColumnData(getIndex());
      column.setEditable(editable);
    }

    public void requestFocus(int rowNumber) {
    }

    public void setKeyword(String keyword) {
      _keyword = keyword.toUpperCase();
    }

    public String getKeyword() {
      return _keyword;
    }

    public void dispose() {
    }

    public java.awt.Component getComponent() {
      return null;
    }
  }

  /**
   * Inner class that manages gaining and losing of focus.  
   */
  class FocusWatcher implements FocusListener {
    /**
     * This method is called when this control gains focus. 
     * 
     * @param e the event that is generated when this control
     *       gains focus
     */
    public void focusGained(FocusEvent e) {
      Component opp = e.getOppositeComponent();
      if(opp == null) {
        //Console.logMessage(this,"focusGain from (null)");
        return;
      }

      //Console.logMessage(this,"focusGain from " + opp.getClass().getName());

      GUIControlEvent event = new 
        GUIControlEvent(ArraySetControl.this, 
          GUIControlEvent.COMPONENT_FOCUS_EVENT,
          null);
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.err.println("Array Focus Exception");  
      } 
    }

    /**
     * This method is called when this control loses focus. 
     * 
     * @param e the event that is generated when this control
     *       loses focus
     */    
    public void focusLost(FocusEvent e) {
      Component opp = e.getOppositeComponent();
      if(opp == null) {
        //Console.logMessage(this, "focusLoss from (null)");
        return;
      }
      //Console.logMessage(this, "focusLoss from " + opp.getClass().getName());

      //if (!_array.isEditing())  {
      if (!(opp instanceof KeyListenerJTextField))  {
        //Console.logMessage(this, "focusLost LEAVE_ARRAYSET_EVENT");
        GUIControlEvent event = new 
          GUIControlEvent(ArraySetControl.this, 
            GUIControlEvent.LEAVE_ARRAYSET_EVENT,
            null);
        try {
          fireGUIControlChanged(event);
        } 
        catch (GUIControlException en) {
          System.err.println("Array Focus Exception");
        }                    
      }
    }
  }

  /**
   * Inner class that is used to send messages to the GUI control listeners
   * about change in the value of this array control.
   */
  class MyTableModelListener implements TableModelListener {
    /**
     * This method is invoked in response to change in the 
     * data contained by the array control.
     * 
     * @param e the event that is generated when the array 
     *       control changes its data
     */
    public void tableChanged(TableModelEvent e) {
      if ( _array.getFlag() ) {
        return;
      }
      _changedColumn = e.getColumn();

      Object key = _indexKeywordLookup.get(new Integer(_changedColumn));

      ColumnWrapper wrapper = (ColumnWrapper) _keywordWrapperLookup.get(key);
      wrapper._localChangedIndex = e.getFirstRow();
      int localIndex = wrapper._localChangedIndex;

      Object o = ((TableModel) e.getSource()).getValueAt(
        localIndex, _changedColumn);

      //SMCook added bypass logic to prevent modify event from being generated
      // when last array element is the one alledgedly changed, and is null.
      // Prevents extraneous additional row from being added by the back end.
      String s = o.toString();
      if(s.equals("")                           &&
         _array.getRowCount() == localIndex + 1 &&
         e.getType() == e.UPDATE) {
        //Console.logMessage(this, "bypassing");
        return;
      }

      GUIControlEvent event = new 
        GUIControlEvent(wrapper, 
          GUIControlEvent.MODIFY_ARRAY_ELEMENT_EVENT,
            s);

      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {
        System.err.println("Array Value Exception");  
      }
    }
  }  

  /**
   * Inner class that is used to send messages to the GUI control listeners
   * in response to item selections and value changes in this control.
   */
  protected class MySelectionListener implements 
    ListSelectionListener, TableColumnModelListener {
    int _currentColumnSelected = -1;
    boolean _ignoreNextColumnSelection = false;

    /**
     * This method is invoked when a selections is changed
     * in this array component.
     * 
     * @param e the event that is generated when the item selection
     *       in this component changes
     */
    public void valueChanged(ListSelectionEvent e) {
      if ( e.getValueIsAdjusting()) {
        return;  
      }
      if (isClickModeImmediate()) {
        ListSelectionModel m = (ListSelectionModel) e.getSource();
        ArrayList al = new ArrayList();

        int imin = e.getFirstIndex();
        int imax = e.getLastIndex();

        for (int i = imin; i <= imax; i++) {
          if (m.isSelectedIndex(i)) {
            al.add(String.valueOf(i+1));  
          }
        }

        int selected = _array.getSelectedColumn();
        Object key = _indexKeywordLookup.get(new Integer(selected));

        if ( _currentColumnSelected != selected) {
          columnSelectionChanged(e);
          _ignoreNextColumnSelection = true;
        }
        else {}

        GUIControlEvent event = new 
          GUIControlEvent(_keywordWrapperLookup.get(key), 
            GUIControlEvent.ITEMS_SELECTED_EVENT,
              al.toArray());
        try {
          fireGUIControlChanged(event);
        }
        catch (GUIControlException en) {
          System.err.println("Array Value Change Exception");  
        }              
      }
    }

    private void dispatchLeaveEvent() {
      Object key = _indexKeywordLookup.get(new Integer(_currentColumnSelected));

      GUIControlEvent event = new 
        GUIControlEvent(_keywordWrapperLookup.get(key), 
          GUIControlEvent.LEAVE_ARRAY_EVENT,
            null);
      try {
        fireGUIControlChanged(event);
      }
      catch (GUIControlException en) {
        System.err.println("Array dispatchLeaveEvent Exception");  
      }              
    }

    public void columnAdded(TableColumnModelEvent e) {
    }

    public void columnMarginChanged(ChangeEvent e) {
    }

    public void columnMoved(TableColumnModelEvent e)  {
    }

    public void columnRemoved(TableColumnModelEvent e)  {
    }

     /**
      * This method is invoked when a column selection changes.
      * 
      * @param e the event that is generated when column
      *       selection changes
      */
    public void columnSelectionChanged(ListSelectionEvent e) {
      if (e.getValueIsAdjusting()) {
        return;
      }
      if (_ignoreNextColumnSelection) {
        _ignoreNextColumnSelection = false;
        return;
      }
      if (_currentColumnSelected != -1 ) {
        dispatchLeaveEvent();
      }
      _currentColumnSelected = _array.getSelectedColumn();
    }
  }  
 
  /**
   * Inner class that sends notification to GUI listeners
   * when an item in the arrayset is clicked more than once.
   */
  protected class MouseHandler extends MouseAdapter {
    /**
     * Method invoked when mouse enters this control.
     */
    public void mouseEntered(MouseEvent e) {
      //Console.logMessage(this, "mouseEntered");
      _mouseIsOutside = false;                   //SMCook added

      GUIControlEvent event = new 
        GUIControlEvent(ArraySetControl.this, 
          GUIControlEvent.MOUSE_ENTERED_EVENT,
            null);
      try {
        fireGUIControlChanged(event);
      }
      catch (GUIControlException ex) {
        System.err.println("Array mouseEntered Exception");  
      }
    }

    /**
     * Method invoked when mouse exits this control.
     */
    public void mouseExited(MouseEvent e) {      //SMCook added.
      //Console.logMessage(this, "mouseExited");
      _mouseIsOutside = true;
    }

    public void mousePressed(MouseEvent e) {
    }

    /**
     * Method invoked when mouse is clicked in this control.
     * added for bug #103, add clicking interaction on non sensitive array.
     */
    public void mouseClicked(MouseEvent e) {
      if (SwingUtilities.isLeftMouseButton(e)) {

        Object o = e.getSource();
        if(!(o instanceof JTable)) return;

        KeyListenerJTable src = (KeyListenerJTable) o;

        Point pt = new Point(e.getX(), e.getY());
        int row = src.rowAtPoint(pt);    
        int col = src.columnAtPoint(pt);

        if ( e.getClickCount() == 1 ) {
          if (!_array.isEnabled()) {
            Object key = _indexKeywordLookup.get(new Integer(col));

            GUIControlEvent event = new 
              GUIControlEvent(_keywordWrapperLookup.get(key), 
                GUIControlEvent.ITEM_CLICKED_EVENT,
                String.valueOf(row + 1));

            try {
              fireGUIControlChanged(event);
            }
            catch (GUIControlException ex) {
              System.err.println("Array mouseClicked Exception");  
            }
          }
        }
        //added for bug #109
        if ( e.getClickCount() == 2 ) {  
          if (!_array.isEnabled()){
            Object key = _indexKeywordLookup.get(new Integer(col));

            GUIControlEvent event = new 
              GUIControlEvent(_keywordWrapperLookup.get(key), 
                GUIControlEvent.DOUBLE_CLICK_SELECTION_EVENT,
                String.valueOf(row + 1));
            try {
              fireGUIControlChanged(event);
            }
            catch (GUIControlException ex) {
              System.err.println("Array mouse clicked Exception");  
            }  
          }
        }
      }
    }
  }

  /**
   * Inner class to listen to mouse clicks on scroll pane.
   */
  protected class MyScrollMouseListener extends MouseAdapter {
    /**
     * Method invoked when mouse is entered in this control.
     */
    public void mousePressed(MouseEvent e) {
      _scroll.setBorder(BorderFactory.createLineBorder(Color.red, 2));
      _array.requestFocus();
      super.mousePressed(e);  //SMCook
    }
  }

  /**
   */
  protected class MyScrollMouseMotionListener extends MouseMotionAdapter {
    /**
     * Method invoked when mouse moves within this control.
     */
    public void mouseMoved(MouseEvent e) {
      Point p = e.getPoint();
      int col = findColumn(p);
      if (col==-1) {
        return;
      }
      Object keyword = _indexKeywordLookup.get(new Integer(col));
      Object wrapper = _keywordWrapperLookup.get(keyword);

      GUIControlEvent event = new 
        GUIControlEvent(wrapper, 
          GUIControlEvent.MOUSE_ENTERED_EVENT,
            null);
      try {
        fireGUIControlChanged(event);
      }
      catch (GUIControlException ex) {
        System.err.println("Array mouseMoved Exception");  
      }
    }  

    /**
     */
    private int findColumn(Point p) {
      Rectangle rowTableRect = _rowTable.getBounds();
      TableColumnModel model = _array.getColumnModel();
      int count = model.getColumnCount();
      if (count == 0) {
        return -1;  
      }
      int start = rowTableRect.width;
      if (p.x <= start) { 
        return -1;
      }
      for (int i=0; i<count; i++) {
        TableColumn current = model.getColumn(i);
        int newStart = start +   current.getWidth();
        if ((p.x>start && p.x<=newStart)) {
          return i;
        } 
        else {
          start = newStart;
        }
      }
      return -1;
    }  
  }
 
  /**
   * Inner class that listens to focus lost event on this array component.
   */      
  protected class InnerListener implements GUIControlListener {
    public void guiControlChanged(GUIControlEvent e) {  
      if ( e.getType() == GUIControlEvent.LEAVE_ARRAYSET_EVENT ) {
        _scroll.setBorder(_defBorder);  
      }
    }
  }

}
