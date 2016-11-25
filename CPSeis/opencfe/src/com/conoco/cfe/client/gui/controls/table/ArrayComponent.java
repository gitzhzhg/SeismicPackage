///
/// ArrayComponent.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
/// 10.
///  9. 09-05-2002 SMCook   Now inherits from KeyListenerJTable.
///                         Removed one constructor, moved stopCellEditing()
///                          functionality to KeyListenerJTable, and made other
///                          miscellaneous changes related to 1.4 upgrade.
///  8. 08-02-2002 SMCook   Make modifications in overrided paint() function to
///                          improve behavior under 1.4.
///                         Made most lines of code adhere to an 80-char limit.
///  7. 10-04-2001 SMCook   Reinstated the use of class ConocoBasicTableUI.
///                         Removed unused code -- FirstClickAction.  This
///                          inner class has been unused at least as far back
///                          as the save_081800 archive.
///                         Removed unused code -- processMouseEvent was doing
///                          nothing but calling super.processMouseEvent.
///                         Removed unused code -- singleClick function and
///                          _singleClickEvent variable were never used (not to
///                          mention that _singleClickEvent was always null). 
///  6. 09-29-2001 SMCook   Added public function stopCellEditing() to allow
///                          other classes to tell the array to stop editing
///                          (part of fix for bug report 514, item 1).
///                         Modified getPreferredSize logic to better detect
///                          when having a horizontal scroll bar is desirable
///                          (addresses bug report 515).
///                         Removed useless EDIT_FLAG variable (always false).
///  5. 09-25-2001 SMCook   Removed debug print statement and added protection
///                          for pathological getPreferredSize situation.
///  4. 09-24-2001 SMCook   Temporary fix for zero-size tables.
///  3. 09-18-2001 SMCook   Replaced ConocoBasicTableUI with Java-supplied
///                          BasicTableUI.  Deleted Conoco version from the
///                          code base.  Eliminated need for certain mouse
///                          listeners.
///                         Fixed horizontal scroll bar annoyance:
///                          Overrided paint()
///                          Overrided getPreferredSize() 
///                          Added _sizeLimiter variable (container size).
///                         Eliminated redundant constructor code.
///

package com.conoco.cfe.client.gui.controls.table;

import com.conoco.shared.Logger;

import com.conoco.cfe.client.Client;
import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.ConocoComboBox;

import com.conoco.cfe.client.gui.controls.table.ConocoBasicTableUI;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Container;
import java.awt.Graphics;

import java.awt.event.FocusEvent;

import java.util.EventObject;

import javax.swing.BorderFactory;
import javax.swing.CellEditor;
import javax.swing.DefaultCellEditor;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
 
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.JTableHeader;

/**
 * A component that wraps around the standard 
 * <code>javax.swing.JTable</code> class. 
 * This component encapsulates the functionality
 * of a table component in which columns can
 * have any one of the following data formats:
 * <ul>
 * <li>Integer</li>
 * <li>Float</li>
 * <li>String</li>
 * </ul>
 * The data is stored in the form of a 
 * <code>com.conoco.cfe.utils.ArrayList</code>.
 * 
 * @see com.conoco.cfe.client.gui.controls.table.ColumnTableModel
 */
public class ArrayComponent extends KeyListenerJTable {
  /**
   * A flag that is set to true if the model
   * of this table component is being changed by the
   * server itself.
   * 
   * @serial
   */
  protected boolean FLAG;
 
  /**
   *
   */
  ColumnTableModel _model;

  /**
   * SMCook - JTable needs to know size of its container to resize intelligently
   */
  protected Container _sizeLimiter;

  /**
   * Constructor 1 of 1.
   */
  public ArrayComponent(String keyword) {
    setAutoResizeMode(AUTO_RESIZE_OFF);

    setUI(new ConocoBasicTableUI());

    setAutoCreateColumnsFromModel(false);
    _model = new ColumnTableModel();

    TableColumnModel _columnModel = new DefaultTableColumnModel();
    _model.setColumnModel(_columnModel);
    setModel(_model);
    setColumnModel(_columnModel);

    setTableHeader(new JTableHeader(_columnModel));   //SMCook

    getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

    setDefaultRenderer(String.class, new ConocoTableCellRenderer());

    getTableHeader().setReorderingAllowed(false);   //SMCook
  }
  
  /**
    * Override getPreferredSize (alter width to match width of sizeLimiter)
    */
  public final Dimension getPreferredSize() {

// safety case for null _sizeLimiter
    Dimension d1,d2=super.getPreferredSize();
    if(_sizeLimiter == null)
      return d2;
    else
      d1=_sizeLimiter.getSize();

// multi-column case
    int imax = _model.getColumnCount();
    if(imax > 1) {
      if(d1.width > 0)
        return new Dimension(d1.width,d2.height);
      else
        return d2;
    }
//
// single-column case (which may need scroll bar)
// use weighted average width of upper- and lower-case letters
//
    TextTableColumn col = _model.getColumn(0).getTableColumn();
    int w = col.getTextField().getFontMetrics(
               col.getTextField().getFont()).charWidth('W');
    w += 9*col.getTextField().getFontMetrics(
               col.getTextField().getFont()).charWidth('s');
    w *= col.getMaxLength();
    w /= 10;

    if(w < d1.width)
      return new Dimension(d1.width,d2.height);
    else
      return new Dimension(w,d2.height);
  }

  /**
   * Needs to know the size of its container to make good size judgments
   */
  public final void setSizeLimiter(Container c) {
    _sizeLimiter = c;
  }

  /**
   * SMCook - Override paint() to assure good column widths
   */
  public final void paint(Graphics g) {

   int i,w;

// adjust column widths based on JTable size
    TableColumn col;

    int imax = _model.getColumnCount();
    if(imax <= 0)
      {
      super.paint(g);
      return;
      }

// calculate total weight
    float weight,totalWeight=0;
    i=0;
    while(i<imax)
      {
      totalWeight += getColumnData(i).getColumnSize();
      i++;
      }

// most columns handled here
    int totalWidth=getWidth();
    int totalUsed=0;
    i = 0;
    while(i < imax-1) {
      weight = getColumnData(i).getColumnSize();
      w = (int)(weight/totalWeight * totalWidth); 
      totalUsed += w;

      col = _model.getColumn(i).getTableColumn();
      col.setMinWidth(w);
      col.setPreferredWidth(w);
      col.setMaxWidth(w);
      i++;
    }

// treat last column differently
    w = totalWidth - totalUsed;

    col = _model.getColumn(i).getTableColumn();
    col.setMinWidth(w+2);
    col.setPreferredWidth(w+2);
    col.setMaxWidth(w+2);

    revalidate();
    super.paint(g);

/* SMCook unnecessary and/or possible source of trouble in 1.4?
    getTableHeader().revalidate();
    getTableHeader().repaint();   //SMCook - to fix sluggish header repaints
*/
  }
  
  /**
   * Adds a column to the table component. 
   * 
   * @param column   the data structure that describes the data in 
   *           the column to be added  
   */
  public void addColumnData(ColumnData column) {
    TextTableColumn tableColumn = column.getTableColumn();
    tableColumn.setModelIndex(getColumnCount());
    ((ColumnTableModel) getModel()).addColumn(column);
    addColumn(tableColumn);
  }
  
  /**
   * Returns the data contained by the table at the specified column index.
   * 
   * @param column the index of the column whose data is desired
   * @return   the data structure that describes the column data 
   *       at the specified index
   */
  public ColumnData getColumnData(int column) {
    return ((ColumnTableModel) getModel()).getColumn(column);
  }

  /**
   * Returns the flag that is set to true if the data model
   * of this table is being modified by the server.
   * 
   * @return   a boolean flag that is <code>true</code> if 
   *       the server modifies the data model of this table;
   *       <code>false</code> otherwise
   */
  public boolean getFlag() {
    return FLAG;  
  }
    
  /**
   * Sets the boolean that indicates whether the data model
   * of this table is being modified by the server or not.
   * 
   * @param b the boolean value
   */
  public void setFlag(boolean b) {
    FLAG = b;  
  }    
    
  /**
   */
  protected void processFocusEvent(FocusEvent e) {

    if ( Client.FOCUS_FLAG ) {   //set by ErrorInfoWarningDialog
      return;  
    }

    super.processFocusEvent(e);
  }

  /**
   * This method is invoked when the user starts editing
   * a table cell.
   * 
   * @param r the row index of the cell that is being edited
   * @param c the column index of the cell being edited
   * @param o the event object that is starts cell editing
   */  
  public boolean editCellAt(int r, int c, EventObject o) {
    FLAG = false;
    boolean ok = super.editCellAt(r, c, o);
    if(ok) {
      Component comp = ((DefaultCellEditor)getCellEditor()).getComponent();
      comp.requestFocus();
    }
    return ok;
  }
  
  public boolean isManagingFocus() {
    return false;
  }
}
