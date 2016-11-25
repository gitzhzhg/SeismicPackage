///
/// ConocoTableCellRenderer.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-05-2002 SMCook   Default cell background is now white if has focus,
///                          not purple (excluding special red/green cases).
///

package com.conoco.cfe.client.gui.controls.table;

import java.awt.Component;
import java.awt.Color;

import javax.swing.JTable;
import javax.swing.BorderFactory;

import javax.swing.table.DefaultTableCellRenderer;

/**
 * A table cell renderer for customizing the UI of 
 * the array and arrayset components. In the new 
 * UI, a selected table cell has a red border.
 */
public class ConocoTableCellRenderer  extends DefaultTableCellRenderer {
  /**
   * Constructs a new table cell renderer.
   */  
  public ConocoTableCellRenderer() {
    super();
  }  

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

    if (isSelected) {
      super.setForeground(table.getSelectionForeground());
      super.setBackground(table.getSelectionBackground());
    }
    else {
      super.setForeground(table.getForeground());
      super.setBackground(table.getBackground());
    }

    setFont(table.getFont());
    if (hasFocus) {
      setBorder(BorderFactory.createLineBorder(Color.red, 2));
      super.setForeground(table.getSelectionForeground());
      Color test = table.getSelectionBackground();
      if((test.getRed() == 255 && test.getGreen() == 0) ||
         (test.getGreen() == 255 && test.getRed() == 0) )
        super.setBackground(table.getSelectionBackground());
      else
        super.setBackground(table.getBackground());
    }
    else {
      setBorder(noFocusBorder);
    }
    setValue(value); 
    return this;
  }
}
