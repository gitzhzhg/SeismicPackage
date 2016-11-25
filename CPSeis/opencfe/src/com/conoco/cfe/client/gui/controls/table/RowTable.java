///
/// RowTable.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-11-2002 SMCook   Added font scaling code to reduce size of fonts.
///

package com.conoco.cfe.client.gui.controls.table;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;

import java.awt.geom.AffineTransform;

import javax.swing.JButton;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;

import javax.swing.event.TableModelListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableCellRenderer;

/**
 * Class to implement row headers in array and arrayset components.
 * The row headers are basically implemented as a table 
 * component with buttons as the cell renderers. The buttons
 * display the row number.
 */
public class RowTable extends JTable implements ChangeListener  {
  /**
   * Declares a variable for the model of the table for 
   * which the row headers are to be provided
   * 
   * @serial
   */
  private TableModel _masterTable;
  
  /**
   * Declares a variable for the renderer that will be used to
   * render the button components.
   * 
   * @serial
   */
  private ButtonRenderer _renderer;
   
  /**
   * Declares a variable for the current font.
   * 
   * @serial
   */
  private Font _font;
  
  /**
   * Constructs a new row table component using the specified
   * table model. The table model is used to get the 
   * number of rows in the table.
   * 
   * @param masterTable the table model to be used
   */
  public RowTable(TableModel masterTable) {
    super();
    _masterTable = masterTable;
    _masterTable.addTableModelListener(  new MasterTableModelListener());
    setModel(new MasterSlaveTableModel());
    _renderer = new ButtonRenderer();
    getColumnModel().getColumn(0).setCellRenderer(_renderer);
  }
  
  /**
   * Sets the current font.
   *
   * @param f the font to be used
   */
  public void setCurrentFont(java.awt.Font f) {
    AffineTransform aff = new AffineTransform();
    aff.setToScale(.8, .8);
    //aff.translate(0, 0);
    _font = f.deriveFont(aff);
    super.setFont(_font);

    //_font = f;
    //super.setFont(f);  
  }
  
  /**
   * Returns the preferred size of this component.
   * 
   * @return the preferred size of this component
   */  
  public Dimension getPreferredSize() {
    int magicBullet = 10;
    Dimension d = super.getPreferredSize();              
    JButton b = (JButton) getColumnModel().getColumn(0).getCellRenderer();
    b.setText(String.valueOf(getModel().getRowCount()));
    d.width = b.getPreferredSize().width + magicBullet;
    return d;
  }
  
  /**
   * This method is invoked when the viewport of the 
   * master table's scrollpane changes.
   */
  public void stateChanged(ChangeEvent e) {
    revalidate();
    repaint();
  }
  
  public boolean isManagingFocus() {
    return false;
  }  
    
  /**
   * A private class to implement the model of the row header 
   * table. 
   */  
  private class MasterSlaveTableModel extends AbstractTableModel {
    /**
     * Returns the number of columns.
     * 
     * @return the number of columns of data contained by this model
     */
    public int getColumnCount() { 
      return 1;
    }
     /**
     * Returns the number of rows in the model.
      * 
      * @return the number of rows of data contained by this model
      */
     public int getRowCount() { 
       return _masterTable.getRowCount();
     }
     /**
      * Returns the data object located at the specified cell.
      * 
      * @param row the row index of the cell
      * @param col the column index of the cell
      * @return the data object
      */
     public Object getValueAt(int row, int col) { 
      return String.valueOf(row+1); 
    }
  }
  
  /**
   * Inner class that implements the renderer for the row table
   * header.
   */
  private class ButtonRenderer extends JButton implements TableCellRenderer {
    /**
     * Constructs a new button renderer object.
     */      
    public ButtonRenderer() {
      super();
    }
    
    /**
     * This method is sent to the renderer by the drawing table to 
     * configure the renderer appropriately before drawing. Return 
     * the Component used for drawing.
     * 
     * @param table    the JTable that is asking the renderer to draw. 
     * This parameter can be null.
     * @param value    the value of the cell to be rendered. It is up to 
     * the specific renderer to interpret and
     * draw the value. eg. if value is the String "true", 
     * it could be rendered as a string or it could be
     * rendered as a check box that is checked. null is a 
     * valid value.
     * @param isSelected true is the cell is to be renderer with selection highlighting
     * @param hasFocus indicates whether the cell has focus or not
     * @param row    the row index of the cell being drawn. When drawing the 
     * header the rowIndex is -1.
     * @param column  the column index of the cell being drawn  
     * @return the component to be used for rendering the cell
     */  
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
                    boolean hasFocus, int row, int column) {
      if ( value != null ) {
        setText((String) value);
      }
      if ( _font != null ) {
        setFont(_font);  
      }
      return this;      
    }      
  }
  
  /**
   * Inner class to listen to changes in the model of the 
   * master table. This is required so that the row table 
   * can repaint itself after rows are inserted or deleted
   * in the master table.
   */
  class MasterTableModelListener implements TableModelListener {
    /**
     * This is invoked when the master table model is changed.
     * This method posts an asynchronous request for 
     * a row table repaint on the event queue.
     * 
     * @param e the event object that is generated when 
     * the master table model changes
     */
    public void tableChanged(TableModelEvent e) {
      repaint();
    }
  }    
}
