///
/// ColumnData.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  5.
///  4. 09-05-2002 SMCook   Constructor now takes variables clickCountToStart
///                          and jtable.
///  3. 09-18-2001 SMCook   Replaced 'configureTableColumn' approach with
///                          approach that just stores the _columnSize.  The
///                          'configuring' is ultimately done at the paint
///                          stage by the JTable instead of in here.
///

package com.conoco.cfe.client.gui.controls.table;

import javax.swing.JTextField;

import javax.swing.table.TableColumn;
import com.conoco.cfe.utils.ArrayList;

/**
 * A data structure that is used to encapsulate data
 * of in a column in a given array component.
 * 
 * @see com.conoco.cfe.client.gui.controls.table.ArrayComponent
 */   
public class ColumnData {
  /**
   * Declares a variable for the data structure that
   * actually stores the data contained by this table
   * component
   * 
   * @serial
   */
  private ArrayList  _data;
 
  /**
   * Declares a boolean variable that is set to true 
   * if the column data is editable
   * 
   * @serial
   */
  private boolean    _editable;
 
  /**
   * Declares a variable for the size (width) of this column.
   * It is used as a weight.
   * 
   * @serial
   */
  private int        _columnSize;
 
  /**
   * Declares a variable for the name of this column.
   * The name is used as a column header in the table
   * 
   * @serial
   */
  private String     _columnName;
 
  /**
   * Declares a variable for the table column 
   * contained by this column data
   * 
   * @serial
   */
  protected TextTableColumn _tableColumn;
 
  /**
   * Declares a variable for the click count needed to start editing.
   * 
   * @serial
   */
  protected int _clickCountToStart;
 
  /**
   * Declares a variable for the click count needed to start editing.
   * 
   * @serial
   */
  protected KeyListenerJTable _jtable;
 
   /**
    * Constructor 1 of 2.
    */
  public ColumnData(int clickCountToStart, KeyListenerJTable jtable) {
    _clickCountToStart = clickCountToStart;
    _jtable = jtable;

    _data = new ArrayList();
    _editable = true;
    _columnName = "Unknown";
    setTableColumn(createTableColumn());
  }
 
   /**
    * Constructor 2 of 2.  Used only for array with ComboBox.
    */
  public ColumnData(String keyword) {
    _data = new ArrayList();
    _editable = true;
    _columnName = "Unknown";
    setTableColumn(createTableColumn(keyword));
  }

  /**
   * Returns the column table object.
   *
   * @return   the column table object
   */
  protected TextTableColumn createTableColumn() {
    return new TextTableColumn(_clickCountToStart, _jtable);
  }

  /**
   * Returns the column table object.
   *
   * @return   the column table object
   */
  protected TextTableColumn createTableColumn(String keyword) {
    return new TextTableColumn(keyword);
  }

  /**
   * Sets the maximum number of characters that can be
   * entered in a given cell in this column data.
   * 
   * @param maxLength the maximum number of characters 
   *           that can be entered in a cell
   *           contained by this column data
   */
  public void setMaxLength(int maxLength) {
    _tableColumn.setMaxLength(maxLength);
    //configureTableColumn();
  }
 
  /**
   * Gets the size (width) desired for the column
   */
  public int getColumnSize() {
    return _columnSize;
  }
 
  /**
   * Sets the size (width) desired for the column
   * 
   * @param columnSize 
   */
  public void setColumnSize(int columnSize, boolean SizeMaxLength) {
//    configureTableColumn(columnSize, SizeMaxLength);  SMCook commented
    _columnSize = columnSize;
  }

  /**
   * Returns the number of rows of data contained by this
   * column.
   * 
   * @return   the number of rows of data contained by this 
   *       column
   */    
  public int getRowCount() {
    return _data.size();
  }
 
  /**
   * Sets the data object at a specified position in this 
   * column data.
   * 
   * @param row   the index of the row at which a data
   *         item is to be set
   */
  public void setValueAt(int row, Object value) {
    _data.ensureCapacity(row+1);
    for (int i=_data.size(); i <= row; i++) {
      _data.add(i, null);
    }
    _data.set(row, value);
  }
 
  /**
   * Returns the data object contained by this table
   * component at the specified row number.
   *
   * @param row   the row index of the cell whose 
   *         data object is desired
   */
  public Object getValueAt(int row) {
    if (row >= _data.size()) {
      return null;
    }
    return _data.get(row);
  }
 
  /**
   * Returns whether the cell located at the specified
   * row number is editable or not.
   * 
   * @param rowNumber the row index of the cell 
   * @return   a boolean value that is <code>true</code>
   *       if the cell is editable; <code>false</code>
   *       otherwise
   */
  public boolean isEditable(int rowNumber) {
    if ( rowNumber < 0 || rowNumber >= getRowCount() ) {
      return false;
    } 
    else {
      return _editable;
    }
  }
 
  /**
   * Sets whether this column data is editable or not.
   * 
   * @param editable   a boolean variable that is <code>true</code>
   *           if this column is to be editable;
   *           <code>false</code> otherwise
   */
  public void setEditable(boolean editable) {
    _editable = editable;
  }
 
  /**
   * Returns the name of this column data. This name
   * is used to draw the column header of this column.
   * 
   * @return the name of this column data
   */
  public String getColumnName() {
    return _columnName;
  }
 
  /**
   * Configures this table column. This method properly 
   * sets the width of the table column based on the 
   * column name.
   */
/*
  protected void configureTableColumn(int columnSize, boolean SizeMaxLength) {
    boolean _SizeMaxLength = SizeMaxLength;
    int wcharWidth = _tableColumn.getTextField().getFontMetrics(
                        _tableColumn.getTextField().getFont()).charWidth('W');
    int mcharWidth = _tableColumn.getTextField().getFontMetrics(
                        _tableColumn.getTextField().getFont()).charWidth('M');          
    if (_SizeMaxLength) {
      int size = _columnName.length();
      if (size < _tableColumn.getMaxLength()) {
        size = _tableColumn.getMaxLength();
      }
      _tableColumn.setMaxWidth(size*(mcharWidth));
      _tableColumn.setMinWidth(size*(mcharWidth));  
    }
    else {
      _tableColumn.setMaxWidth(columnSize*(mcharWidth));
      _tableColumn.setMinWidth(columnSize*(mcharWidth));
    }
  }
*/
 
  /**
   * Sets the name of this column data.
   * 
   * @param columnName the name of this column data
   */
  public void setColumnName(String columnName) {
    _columnName = columnName;
  }
 
  /**
   * Returns the table column object contained by this 
   * column data.
   * 
   * @return the table column object contained by this column data
   */
  public TextTableColumn getTableColumn() {
    return _tableColumn;
  }
 
  /**
   * Sets the table column object.
   * 
   * @param tableColumn   the table column object to be set 
   *             on this column data
   */
  public void setTableColumn(TextTableColumn tableColumn) {
    _tableColumn = tableColumn;
  }
 
  /**
   * Returns the data contained by this column data.
   * 
   * @return   the data contained by this column as 
   *       <code>com.conoco.cfe.utils.ArrayList</code>
   */
  public ArrayList getData() {
    return _data;
  }

  /**
   * Inserts a row at a specified position.
   * 
   * @param rowNumber the index of the position where 
   *           a row is to be inserted
   * @param rowData the object to be inserted
   */
  public void insertRow(int rowNumber, Object rowData) {
    _data.add(rowNumber, rowData);
  }
 
  /**
   * Removes a specified row from this table.
   * 
   * @param number the index of the row which is to be removed
   */
  public void removeRow(int number) {
    _data.remove(number);  
  }
}
