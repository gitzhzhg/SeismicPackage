///
/// IntegerColumnData.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-05-2002 SMCook   Constructor and createTableColumn() now utilize
///                          variables clickCountToStart and jtable.
///


package com.conoco.cfe.client.gui.controls.table;

/**
 * Creates a column data that accepts only integer values. 
 * The column data object is used to encapsulate the data 
 * in columns of array components.
 * 
 * @see com.conoco.cfe.client.gui.controls.table.ArrayComponent
 * @see com.conoco.cfe.client.gui.controls.table.ColumnTableModel
 */  
public class IntegerColumnData extends ColumnData {

  /**
   * Constructor 1 of 1.
   */
  public IntegerColumnData(int clickCountToStart, KeyListenerJTable jtable) {
    super(clickCountToStart, jtable);
  }

  /**
   * Returns the column table object.
   *
   * @return   the column table object
   */
  protected TextTableColumn createTableColumn() {
    return new IntegerTableColumn(_clickCountToStart, _jtable);
  }
}
