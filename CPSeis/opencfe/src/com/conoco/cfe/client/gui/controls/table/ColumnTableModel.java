///
/// ColumnTableModel.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-05-2002 SMCook   Added protective code to catch possible exception
///                          in removeRow() function;
///

package com.conoco.cfe.client.gui.controls.table;

import com.conoco.cfe.client.application.Console;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumnModel;
 
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import com.conoco.cfe.utils.ArrayList;

/**
 * A class to implement the model of an array component that
 * uses a <code>javax.swing.JTable</code> as its UI component.
 * 
 * @see com.conoco.cfe.client.gui.controls.table.ArrayComponent
 */
public class ColumnTableModel extends AbstractTableModel  {
  /**
   * Declares a variable for the column data objects 
   * contained by this model
   * 
   * @serial
   */
  protected ArrayList      _columns;
  
  /**
   * Declares a variable for the maximum number of rows 
   * of data that can be contained by this model
   *
   * @serial
   */
  protected int        _maxRows=0;
  
  /**
   * Declares a variable for the table column model
   *
   * @serial
   */
  protected TableColumnModel  _columnModel;
  
  /**
   * Constructs a new table model.
   */
  public ColumnTableModel() {
    _columns = new ArrayList();
  }
  
  /**
   * Returns the data structure that stores the column
   * data.
   * 
   * @param col the index of the column whose data is desired
   */
  public ColumnData getColumn(int col) {
    return (ColumnData) _columns.get(col);
  } 
  
  /**
   * Adds a column to this model.
   * 
   * @param column   the data structure that stores the 
   *           column data
   */
  public void addColumn(ColumnData column) {
    _columns.add(column);
  }
  
  /**
   * Returns data for specified column index.
   * 
   * @param index the index of the column whose data is desired
   * @return the data as an array of <code>java.lang.String</code>
   */
  public String[] getDataForIndex(int index) {
    ColumnData colData = getColumn(index);
    ArrayList list = colData.getData();
    if (list.size() == 0) {
      return null;
    }
    else {  
      return (String [] ) list.toArray();  
    }
  }
  
  /**
   * Calculates the maximum number of rows of data. The table columns
   * can have variable rows of data in them. This method
   * calculates the maximum number of rows in the table model.
   */
  protected void calcMaxRows() {
    _maxRows = 0;
    if (_columnModel == null) {
      return;
    }
    for (int i=0; i < _columnModel.getColumnCount(); i++) {
      ColumnData column = getColumn(i);
      if (column.getRowCount() > _maxRows) {
        _maxRows = column.getRowCount();
      }
    }
  }
  
  /**
   * Sets the column model.
   * 
   * @param columnModel the column model to be used
   */ 
  public void setColumnModel(TableColumnModel columnModel) {
    _columnModel = columnModel;
  }
  
  /**
   * Returns the column model attached to this table model.
   * 
   * @return the column model that is used by this table model
   */
  public TableColumnModel getColumnModel() {
    return _columnModel;
  }
  
  /**
   * Returns the number of columns in this model.
   * 
   * @return the number of columns in this model
   */
  public int getColumnCount() { 
    return _columns.size();
  }
    
  /**
   * Returns the row count in this model. This returns the 
   * maximum number of row count in this model.
   * 
   * @return the number of rows in this model
   */
  public int getRowCount() { 
    calcMaxRows();
    return _maxRows;
  }
    
  /**
   * Returns the value at the specified position.
   * 
   * @param row the row index 
   * @param col the column index
   * @return the data object at the specified cell
   */
  public Object getValueAt(int row, int col) { 
    return getColumn(col).getValueAt(row); 
  }
    
  /**
   * Returns whether a specified cell is editable or not.
   * 
   * @param row the row index 
   * @param col the column index
   * @return   a boolean value that is set to <code>true</code>
   *       if cell is editable; <code>false</code> otherwise
   */
  public boolean isCellEditable(int row, int col) { 
    return getColumn(col).isEditable(row); 
  }
    
  /**
   * Returns the column class for the specified column number.
   * 
   * @param col the index of the column 
   * @return the class of the column specified
   */
  public Class getColumnClass(int col) {
    return String.class; 
  }
    
  /**
   * Sets the value at the specified cell.
   * 
   * @param value to be set at the specified cell
   * @param row the row index 
   * @param col the column index
   */  
  public void setValueAt(Object value, int row, int col) {
    getColumn(col).setValueAt(row, value);
    fireTableCellUpdated(row, col);
  }
    
  /**
   * Returns the name of the column at the specified index.
   * 
   * @param col the index of the column
   * @return the name of the column as a <code>java.lang.String</code>
   */
  public String getColumnName(int col) {
    return getColumn(col).getColumnName();
  }
    
  /**
   * Notifies the listeners that a specified table cell has been
   * updates.
   * 
   * @param r the row index of the cell updated
   * @param c the column index of the cell updated
   */
  public void fireTableCellUpdated(int r, int c) {
    TableModelEvent event = new TableModelEvent(this, r, r, c, 
                                  TableModelEvent.UPDATE);
    Object[] listeners = listenerList.getListenerList();
    for ( int i = 0; i < listeners.length; i++) {
      if ( listeners[i] instanceof TableModelListener ) {
        ((TableModelListener) listeners[i]).tableChanged(event);  
      }
    }
  }
    
  /**
   * Inserts a row of data objects.
   * 
   * @param row the index of the position where a row is to be inserted
   * @param data the array of data objects to be inserted
   */
  public void insertRow(int row, Object[] data) {
    for ( int i = 0; i < _columns.size(); i++) {
       ((ColumnData) _columns.get(i)).insertRow(row, data[i]);  
     }  
    fireTableRowsInserted(row, row, -1);
  }
    
  /**
   * Inserts a single data object at a specified position.
   * 
   * @param row the row index of the cell
   * @param col the column index of the cell
   * @param obj the data object to be inserted
   */
  public void insertRow(int row, int col, Object obj) {
    ColumnData data = getColumn(col);
    data.insertRow(row, obj);
    fireTableRowsInserted(row, row, col);  
  }
    
  /**
   * Removes a row at the specified position.
   * 
   * @param number the index of the row to be removed
   */
  public void removeRow(int number) {
    try {
      for ( int i = 0; i < _columns.size(); i++) {
        ((ColumnData) _columns.get(i)).removeRow(number);  
      }
      fireTableRowsDeleted(number, number, -1);  
    }
    catch(Exception e) {
      Console.logMessage(this, "exception removing col number = " + number);
    }
  }

  /**
   * Removes a specified cell.
   * 
   * @param row the row index of the cell to be removed
   * @param col the column index of the cell to be removed
   */
  public void removeRow(int row, int col) {
    ColumnData data = getColumn(col);
    data.removeRow(row);
    fireTableRowsDeleted(row, row, col);  
  }
     
  /**
   * Notifies listeners when rows are inserted in the model.
   * 
   * @param s the start index
   * @param e the end index
   * @param col the index of the column
   */
  public void fireTableRowsInserted(int s, int e, int col) {
    TableModelEvent event = null;
    if ( col == -1 ) {
      event = new TableModelEvent(this, s, e,
                    TableModelEvent.ALL_COLUMNS, 
                      TableModelEvent.INSERT);    
    } 
    else {
      event = new TableModelEvent(this, s, e,
                    col, TableModelEvent.INSERT);    
    }
    Object[] listeners = listenerList.getListenerList();
    for ( int i = 0; i < listeners.length; i++) {
      if ( listeners[i] instanceof TableModelListener ) {
        ((TableModelListener) listeners[i]).tableChanged(event);  
      }
    }
  }
    
  /**
   * Notifies listeners when rows are deleted from the model.
   * 
   * @param s the start index
   * @param e the end index
   * @param col the index of the column
   */
  public void fireTableRowsDeleted(int s, int e, int col) {
    TableModelEvent event = null;
    if ( col == -1 ) {
      event = new TableModelEvent(this, s, e,
                    TableModelEvent.ALL_COLUMNS, 
                      TableModelEvent.DELETE);    
    } 
    else {
      event = new TableModelEvent(this, s, e,
                    col, TableModelEvent.DELETE);    
    }
    Object[] listeners = listenerList.getListenerList();
     for ( int i = 0; i < listeners.length; i++) {
       if ( listeners[i] instanceof TableModelListener ) {
        ((TableModelListener) listeners[i]).tableChanged(event);  
      }
    }
  }
}
