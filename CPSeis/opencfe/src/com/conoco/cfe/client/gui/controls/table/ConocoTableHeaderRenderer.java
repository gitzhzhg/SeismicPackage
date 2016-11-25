///
/// ConocoTableHeaderRenderer.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 08-02-2002 SMCook   Added one space to pad beginning of header label in
///                          order to improve appearance of header.  Left edge
///                          of first character was getting chopped off.
///

package com.conoco.cfe.client.gui.controls.table;

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.UIManager;

import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * Class that provides the renderer component for the
 * table header. The renderer is used to implement a 
 * table header which changes color depending on whether
 * the table is enabled or not.
 */
public class ConocoTableHeaderRenderer extends DefaultTableCellRenderer {
  /**
   * Returns the table cell renderer component.
   * 
   * @param table the table component on which this rendere will act
   * @param value the value in the cell
   * @param isSelected   a boolean variable for determining whether 
   *             a cell is selected or not
   * @param hasFocus     a boolean variable for determining whether
   *             a cell has focus or not
   * @param row the row number of the cell
   * @param column the column number of the cell
   * @return the component that will be used to render the cell
   */  
  public Component getTableCellRendererComponent(JTable table, Object value,
          boolean isSelected, boolean hasFocus, int row, int column) {
    if (table != null) {
      JTableHeader header = table.getTableHeader();
      if (header != null) {
         if ( table.isEnabled() ) {
           setForeground(header.getForeground());
        } 
        else {
          setForeground(java.awt.Color.gray);
        }
        setBackground(header.getBackground());
        setFont(header.getFont());
      }
    }

    setText((value == null) ? "" : " " + value.toString());  //SMCook padded

    setBorder(UIManager.getBorder("TableHeader.cellBorder"));
    return this;
  }    
}
  
