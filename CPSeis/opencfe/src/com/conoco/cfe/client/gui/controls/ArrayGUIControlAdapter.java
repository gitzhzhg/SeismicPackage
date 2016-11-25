///
/// ArrayGUIControlAdapter.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  8.
///  7. 09-27-2002 SMCook   Added protective code to prevent MODIFY event from
///                          being sent under certain conditions.
///  6. 09-05-2002 SMCook   Made miscellaneous changes supporting 1.4 upgrade.
///  5. 08-13-2002 SMCook   Added protective code to prevent SelectedBackground
///                          color from ever being null.
///  4. 10-04-2001 SMCook   Took function calls out loop in valueChanged().
///  3. 09-18-2001 SMCook   Changed comments so they don't use '///'. 
///

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.table.ArrayComponent;
import com.conoco.cfe.client.gui.controls.table.ColumnData;
import com.conoco.cfe.client.gui.controls.table.ColumnTableModel;

import com.conoco.cfe.utils.ArrayList;

import java.awt.Component;
import java.awt.Font;
import java.awt.Cursor;
import java.awt.Event;
import java.awt.Point;
import java.awt.Color;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.KeyStroke;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.DefaultCellEditor;
import javax.swing.ListSelectionModel;
import javax.swing.BorderFactory;

import javax.swing.event.TableModelListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;

import javax.swing.table.TableModel;
import javax.swing.table.TableColumn;
 
/**
 * An adapter class for an array control. This adapter can be extended
 * to create new array controls. 
 * 
 * @see com.conoco.cfe.client.gui.controls.beans.ArrayComponent
 */
public class ArrayGUIControlAdapter extends GUIControlAdapter
  implements ArrayGUIControl {
  /**
   * The array component which is encapsulated by this control.
   * 
   * @serial
   */
  protected ArrayComponent _array;

  /**
   * Decalres a variable to store the index of the element
   * that has been changed.
   * 
   * @serial
   */
  protected int _changedIndex;

  /**
   * Decalres a variable to store the column index of the element
   * that has been changed.
   * 
   * @serial
   */
  protected int _changedColumn;
  
  /**
   * Variable for the table model listener.
   * 
   * @serial
   */
  protected TableModelListener _tableModelListener;
    
  /**
   * Variable for the double click Listener
   * 
   * @serial
   */
  protected MouseListener _mouseListener;
  
  /**
   * Variables for the different keyboard actions
   * 
   * @serial
   */
  protected ActionListener _insertAction, _deleteAction, _copyAction, _pasteAction;
  
  /**
   */
  protected boolean _isClickModeImmediate;
  
  /**
   */
  protected PopupMenuControl _popup;

  /**
   */     
  protected ListSelectionListener _selectionListener;
  
  /**
   */
  public static Color SelectedBackground = ClientConstants.HLIGHT3;  //SMCook
  
  /**
   * Constructor 1 of 2 (uses blank (i.e. "") for keyword arg).
   */
  public ArrayGUIControlAdapter() {
    _array = new ArrayComponent("");
    _array.setRowSelectionAllowed(true);
    ListSelectionModel selModel = _array.getSelectionModel();
    selModel.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    addFocusListener();
    addModelListener();
    addKeyListener();
    addMouseListener();
    addListSelectionListener();
  }
 
  /**
   * Constructor 2 of 2.
   */      
  public ArrayGUIControlAdapter(String keyword) {
    _array = new ArrayComponent(keyword);
    _array.setRowSelectionAllowed(true);
    ListSelectionModel selModel = _array.getSelectionModel();
    selModel.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    addFocusListener();
    addModelListener();
    addKeyListener();
    addMouseListener();
    addListSelectionListener();
  }

  /**
   * Indicates whether the click mode is immediate
   * on this array component. If the click mode is true,
   * the array component will respond to 
   * the mouse events as defined in the specifications. 
   * 
   * @return the click mode
   */
  public boolean isClickModeImmediate() {
    return _isClickModeImmediate;
  }
  
  /**
   * Sets the click mode on this array component.
   * If the click mode is true,
   * the array component will respond to 
   * the mouse events as defined in the specifications. 
     *
   * @param mode the click mode 
   */
  public void setClickModeImmediate(boolean mode) {
    _isClickModeImmediate = mode;
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
   * Adds the list selection listener on this table.
   */
  protected void addListSelectionListener() {
    _selectionListener = new MySelectionListener();
    _array.getSelectionModel().addListSelectionListener(_selectionListener);
  }  
    
  /**
   * Adds the model listener on the array component.
   */
  protected void addModelListener() {
    _tableModelListener = new MyTableModelListener();
    _array.getModel().addTableModelListener(_tableModelListener);
    _array.getModel().addTableModelListener(_array);
  }
  
  /**
   * Adds the focus listener.
   */
  protected void addFocusListener() {
    _array.addFocusListener(new FocusWatcher());
  }
  
  /**
   * Adds a mouse listener on the array for double clicks
   */
  protected void addMouseListener() {
    _mouseListener = new MouseHandler();
    registerMouseListener();
  }
  
  /**
   * Registers the mouse listener.
   */
  protected void registerMouseListener() {
    _array.addMouseListener(_mouseListener);
  }
  
  /**
   * Adds a key listener on the array component
   */
  protected void addKeyListener() {
    _insertAction = new InsertAction();
    _deleteAction = new DeleteAction();
    _pasteAction = new PasteAction();    //SMCook - apparently never used
    _copyAction = new CopyAction();
    addMutantActions();
  }
  
  /**
   * Registers different keyboard actions with the table 
   * component.
   */
  protected void addMutantActions()  {
    //register action for ctrl+I
    _array.registerKeyboardAction( _insertAction, "Insert",
      KeyStroke.getKeyStroke('I', Event.CTRL_MASK),
      JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    //register action for Insert      
    _array.registerKeyboardAction( _insertAction, "Insert",
      KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, 0),
      JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    //register action for ctrl+D
    _array.registerKeyboardAction( _deleteAction, "Delete",
      KeyStroke.getKeyStroke('D', Event.CTRL_MASK),
      JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    //register action for ctrl+R
    _array.registerKeyboardAction( _deleteAction, "Delete",
      KeyStroke.getKeyStroke('R', Event.CTRL_MASK),
      JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    //register action for Delete      
    _array.registerKeyboardAction( _deleteAction, "Delete",
      KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0),
      JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
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
    _array.setFlag(true);
    TableModel model = _array.getModel();    
    for ( int i = start; i <= end; i++) {
      model.setValueAt(elements[i], i, 0);  
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
    _array.setFlag(true);
    ColumnTableModel tableModel = (ColumnTableModel) _array.getModel();
    Object[] rowData = new Object[1];
    for ( int i = 0; i < elements.length; i++) {
      rowData[0] = elements[i];
      tableModel.insertRow(start+i, rowData);
    }
    _array.setFlag(false);    
  }
  
  /**
   * To fill the combo component in the array.
   * 
   * @param elements   values to set in the combo box component
   */
  public void fillArrayCombo(String[] elements){
    TableColumn column = _array.getColumnModel().getColumn(0);
    DefaultCellEditor cellEditor = (DefaultCellEditor) column.getCellEditor();
    Component cellEditorComponent = cellEditor.getComponent();
    if (cellEditorComponent instanceof JComboBox){
      JComboBox combo = (JComboBox) cellEditorComponent;
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
    _array.setFlag(true);
    ColumnTableModel tableModel = (ColumnTableModel) _array.getModel();
    if ( tableModel.getRowCount() != 0 ) {
      for ( int i = end; i >= start; i--) {
        try {
          tableModel.removeRow(i);
        } catch(Exception e) {
          Console.logMessage(this, "deleteElements " + start + " " + end);
        }
      }
    }
    _array.setFlag(false);
  }

  /**
   * Returns the elements contained by this array component.
   * 
   * @return   the elements contained by this array component
   *       as an array of <code>java.lang.String</code>
   */
  public String[] getElements() {
    ColumnTableModel model = (ColumnTableModel) _array.getModel();
    return model.getDataForIndex(0);
  }
  
  /**
   * Returns the number of rows in the array control.
   * 
   * @return the number of items in the array control
   */
  public int getRowCount() {
    ColumnTableModel model = (ColumnTableModel) _array.getModel();
    ColumnData data = model.getColumn(0);
    return data.getRowCount();
  }

  /**
   * Notifies the listeners that the GUI control has changed state.
   * 
   * @param e the event that is generated when this control changes
   *       state
   */
  protected void fireGUIControlChanged(GUIControlEvent e) throws GUIControlException {
    _array.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    for(int i=0; i<_listeners.size(); i++) {
      ((GUIControlListener) 
        _listeners.get(i)).guiControlChanged(e);
    }  
    _array.setCursor(Cursor.getDefaultCursor());
  }
  
  /**
   * Causes this control to gain focus.
   * 
   * @param rowNumber the number of the row that is to gain focus 
   */
  public void requestFocus(int rowNumber) {
    _array.getSelectionModel().setSelectionInterval(rowNumber, rowNumber);
    _array.requestFocus();
  }
  
  /**
   * Set the Selection background color.
   * action after SetArrayColorBackground message is sent.
   * 
   * @param s string that defines the color desired. 
   */
  public static void setSelectedColor(String s){
    if (s.equals("HLIGHT1")){
      SelectedBackground = ClientConstants.HLIGHT1;
    }
    else if(s.equals("HLIGHT2")){
      SelectedBackground = ClientConstants.HLIGHT2;
    }
    else if(s.equals("HLIGHT3")){
      SelectedBackground = ClientConstants.HLIGHT3;
    }
  }  
  
  /**
   * Clear the selected cells.
   * action after ClearSelection message is sent.
   * 
   */
  public  void clearSelection(){
    _array.clearSelection();
  }  

  /**
   * Sets he font on this control.
   * 
   * @param f the desired font
   */
  public void setFont(Font f) {
    _array.setFont(f);    
    _array.getTableHeader().setFont(f);
  }
    
  /**
   * Sets whether this control is editable or not.
   * 
   * @param editable   the boolean flag that is true if
   *           this control is to be editable; false otherwise
   */
  public void setEditable(boolean editable) {
    for (int i=0; i < _array.getColumnCount(); i++) {
      setColumnEditable(editable, i);
    }
    if ( editable == false ) {
      _array.resetKeyboardActions();  
    } 
    else {
      addMutantActions();  
    }
  }        
  
  /**
   * Sets the editability of a specified column.
   *
   * @param editable specifies whether a column is editable or not
   * @param index the column index
   */
  protected void setColumnEditable(boolean editable, int index) {
    ColumnData data = _array.getColumnData(index);
    data.setEditable(editable);  
  }
  
  /**
   * Sets the sensitivity of this control.
   * 
   * @param sensitive the boolean flag that is true if 
   *           this control is to be sensitive; false otherwise
   */
  public void setSensitive(boolean sensitive) {
    _array.setEnabled(sensitive);
    if ( sensitive ) {
      _array.setBackground(java.awt.Color.white);
    } 
    else {
      _array.setBackground(new java.awt.Color(204, 204, 204));
    }
    _array.repaint();
  }
   
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _array;
  }

  /**
   * Returns the array component contained by this array control.
   *
   * @return the array component 
   */
  public ArrayComponent getArrayComponent() {
    return _array;
  }
    
  /**
   * Returns the index of the array element that has been 
   * modified.
   * 
   * @return the index of the element that has been modified
   */
  public int getChangedIndex() {
    return _changedIndex;
  }
  
  /**
   * Returns the column index of the element that has been changed
   * 
   * @return the column index of the element that has been changed
   */
  public int getChangedColumn() {
    return _changedColumn;
  }
  
  /**
   * Returns the column header.
   * 
   * @return the column header string
   */
  public String getColumnName() {
    ColumnData column =  _array.getColumnData(0);
    return column.getColumnName();  
  }
  
  /**
   * Disposes this control.
   */
  public void dispose() {
    _array.getModel().removeTableModelListener(_tableModelListener);
    _array.getSelectionModel().removeListSelectionListener(_selectionListener);
    for (int i =0; i < getRowCount(); i++) {
      try {
        ((ColumnTableModel) _array.getModel()).removeRow(i);
      } catch(Exception e) {
          Console.logMessage(this, "dispose exception");
      }
    }
    super.dispose();
  }
 
 
  //INNER CLASSES

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
      GUIControlEvent event = new 
        GUIControlEvent(ArrayGUIControlAdapter.this, 
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
      //focus is lost when cell is editing
      if ( !_array.isEditing())  {
        GUIControlEvent event = new 
          GUIControlEvent(ArrayGUIControlAdapter.this, 
            GUIControlEvent.LEAVE_ARRAY_EVENT,
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
      _changedIndex = e.getFirstRow();
      _changedColumn = e.getColumn();

      Object o = ((TableModel) e.getSource()).
        getValueAt(_changedIndex, _changedColumn);

      //SMCook added bypass logic to prevent modify event from being generated
      // when last array element is the one alledgedly changed, and is null.
      // Prevents extraneous additional row from being added by the back end.
      String s = o.toString();
      if(s.equals("")                              &&
         _array.getRowCount() == _changedIndex + 1 &&
         e.getType() == e.UPDATE) {
        //Console.logMessage(this, "bypassing");
        return;
      }

      GUIControlEvent event = new 
        GUIControlEvent(ArrayGUIControlAdapter.this, 
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
   * Inner class that implements the "InsertElements" action.
   * The key listener sends a insert notification.
   */
  protected class InsertAction implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      _array.stopCellEditing();

      JTable src = (JTable) e.getSource();
      int row = src.getSelectedRow();
      int col = src.getSelectedColumn();

      GUIControlEvent event = new 
        GUIControlEvent(ArrayGUIControlAdapter.this, 
          GUIControlEvent.INSERT_ELEMENT_EVENT,
          String.valueOf(_array.getSelectedRow() + 1));
          
      try {
        fireGUIControlChanged(event);
      }
      catch (GUIControlException ex) {
        Console.logMessage(this, ex.getMessage());
      }
    }
  }
  
  /**
   * Inner class that implements the "DeleteElements" action.
   * The key listener sends a delete notification.
   */
  protected class DeleteAction implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      _array.stopCellEditing();

      JTable src = (JTable) e.getSource();
      int row = src.getSelectedRow();
      int col = src.getSelectedColumn();

      GUIControlEvent event = new 
        GUIControlEvent(ArrayGUIControlAdapter.this, 
          GUIControlEvent.REMOVE_ELEMENT_EVENT,
          String.valueOf(_array.getSelectedRow() + 1));

      if(row < 0 || col < 0) return;  //SMCook

      try {
        fireGUIControlChanged(event);
        _array.moveTo(row, col, true, this);
      }
      catch (GUIControlException ex) {
        Console.logMessage(this, ex.getMessage());
      }
    }
  }

  /**
   * Inner class that implements the "PasteElements" action.
   * The key listener sends a paste notification.
   */
  protected class PasteAction implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      ListSelectionModel m = (ListSelectionModel) _array.getSelectionModel();
      ArrayList al = new ArrayList();
      int count = 0;
      for (int i=m.getMinSelectionIndex(); i <= m.getMaxSelectionIndex(); i++) {
        if (m.isSelectedIndex(i)) {
          ++count;
          al.add(String.valueOf(i+1));  
        }
      }    
      if ( count != 0 ) {
        GUIControlEvent event = new 
          GUIControlEvent(ArrayGUIControlAdapter.this, 
            GUIControlEvent.PASTE_ELEMENTS_EVENT,
            al.toArray());
        try {
          fireGUIControlChanged(event);
        }
        catch (GUIControlException ex) {
        System.err.println("Array Paste Action Exception");  
        } 
      }  
    }
  }

  /**
   * Inner class that implements the "CopyElements" action.
   * The key listener sends a copy notification.
   */
  protected class CopyAction implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      ListSelectionModel m = (ListSelectionModel) _array.getSelectionModel();

      int imin = m.getMinSelectionIndex();
      int imax = m.getMaxSelectionIndex();
      if(imin < 0)    return;
      if(imax < imin) return;

      ArrayList al = new ArrayList();
      for (int i = imin; i <= imax; i++) {
        if (m.isSelectedIndex(i)) {
          al.add(String.valueOf(i+1));  
        }
      }      

      GUIControlEvent event = new 
        GUIControlEvent(ArrayGUIControlAdapter.this, 
          GUIControlEvent.ITEMS_SELECTED_EVENT,
            al.toArray());
          
      try {
        fireGUIControlChanged(event);
      }
      catch (GUIControlException ex) {
        System.err.println("Array Copy Action Exception");  
      }
    }
  }
  
  /**
   * Inner class that is used to send messages to the GUI control listeners
   * in response to item selections and value changes in this control.
   */
  protected class MySelectionListener implements ListSelectionListener {
    /**
     * This method is invoked when a selections is changed
     * in this array component.
     * 
     * @param e the event that is generated when the item selection
     *       in this component changes
     */
    public void valueChanged(ListSelectionEvent e) {
      if (e.getValueIsAdjusting()) {
        return;  
      }
      ListSelectionModel m = (ListSelectionModel) _array.getSelectionModel();

      int imin=m.getMinSelectionIndex();
      int imax=m.getMaxSelectionIndex();
      if(imin < 0)    return;
      if(imax < imin) return;

      ArrayList al = new ArrayList();
      for (int i = imin; i <= imax; i++) {
        if (m.isSelectedIndex(i)) {
          al.add(String.valueOf(i+1));  
        }
      }

      GUIControlEvent event = new 
        GUIControlEvent(ArrayGUIControlAdapter.this, 
          GUIControlEvent.ITEMS_SELECTED_EVENT,
            al.toArray());
          
      try {
        fireGUIControlChanged(event);
      }
      catch (GUIControlException ex) {
        System.err.println("Array selection listener Exception");  
      }

      try {
        if(_array == null) {
          Console.logMessage(this, "_array was null");
        }
        else {
          _array.setSelectionBackground(SelectedBackground);
        }
      }
      catch (Exception ex) {
        Console.logMessage(this, "ex:\n" + ex.getMessage());
      }
    }
  }
    
  /**
   * Inner class that listens to item clicks in the
   * array component.
   */
  protected class MouseHandler extends MouseAdapter {
    /**
     * Method invoked when mouse is entered in this control.
     */
    public void mouseEntered(MouseEvent e) {
      GUIControlEvent event = new 
        GUIControlEvent(ArrayGUIControlAdapter.this, 
          GUIControlEvent.MOUSE_ENTERED_EVENT,
            null);
      try {
        fireGUIControlChanged(event);
      }
      catch (GUIControlException ex) {
        System.err.println("Array mouse entered Exception");  
      }
    }
    
    /**
     * Method invoked when mouse is clicked on this control.
     */
    public void mouseClicked(MouseEvent e) {
      if (SwingUtilities.isLeftMouseButton(e)) {

        //selection on non editable array
        if (!_array.isEnabled()){
          JTable src = (JTable) e.getSource();
          Point pt = new Point(e.getX(), e.getY());
          int row = src.rowAtPoint(pt);    

          if (e.getClickCount() == 1) {

            GUIControlEvent event = new 
              GUIControlEvent(ArrayGUIControlAdapter.this, 
                GUIControlEvent.ITEM_CLICKED_EVENT,
                String.valueOf(row + 1));
            try {
              fireGUIControlChanged(event);
            }
            catch (GUIControlException ex) {
              System.err.println("Array mouse clicked Exception");  
            }        
          }
          else if (e.getClickCount() == 2) {            //added for bug #109
            
            GUIControlEvent event = new 
              GUIControlEvent(ArrayGUIControlAdapter.this, 
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
      
      else if (SwingUtilities.isMiddleMouseButton(e) ||
               SwingUtilities.isRightMouseButton(e))    {
        if (e.getClickCount()==1) {  
          if (isClickModeImmediate()) {
            //show popup menu
            if ( _popup != null ) {
              _popup.show( e.getComponent(), e.getX(), e.getY());
            }
          }
        } 
      }
    }
  }
}
