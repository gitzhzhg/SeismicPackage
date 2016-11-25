///
/// KeyListenerJTable.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  3.
///  2. 10-22-2002 SMCook   Removed System.exit call for case when edit fails
///                          (relict debug code).
///  1. 09-05-2002 SMCook   Original version.  Created for the purpose of
///                          having clearer editing functionality in arrays.
///

package com.conoco.cfe.client.gui.controls.table;

import com.conoco.cfe.client.application.Console;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.CellEditor;
import javax.swing.DefaultCellEditor;
import javax.swing.JTable;
import javax.swing.JTextField;

import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableModelEvent;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

public abstract class KeyListenerJTable
  extends JTable implements FocusListener {

  private int _preferredRow = -1;
  private int _preferredCol = -1;

  private Component _opposite;

  public KeyListenerJTable() {
    addFocusListener(this);
  }

  public void setSelectedRow(int row) {
    getSelectionModel().setSelectionInterval(row, row);
  }

  public void setSelectedColumn(int col) {
    getColumnModel().getSelectionModel().setSelectionInterval(col, col);
  }

  public void setPreferredRow(int row) {
    _preferredRow = row;
  }

  public void setPreferredCol(int col) {
    _preferredCol = col;
  }

  public final void stopCellEditing() {
    CellEditor editor=getCellEditor();
    if(editor!=null) editor.stopCellEditing();
  }

  public void moveTo(int row, int col, boolean edit, Object src) {
    //Console.logMessage(this, "movin' to row, col = " + row + ", " + col);
    //Console.logMessage(this, "src was " + src.getClass().getName());

    scrollRectToVisible(getCellRect(row, col, true));

    setSelectedRow(row);
    setSelectedColumn(col);

    if(edit) {
      boolean success = editCellAt(row, col);
      if(!success) {
        //System.err.println("editCellAt failed"); System.exit(1);
      }
    }
  }

  public void moveLeft(boolean edit) {
    int row = getSelectedRow();
    int col = getSelectedColumn() - 1;
    if(col < 0) col = 0;
    setPreferredRow(row);
    setPreferredCol(col);
    moveTo(row, col, edit, this);
  }

  public void moveRight(boolean edit) {
    int row = getSelectedRow();
    int col = getSelectedColumn() + 1;
    if(col > getColumnCount() - 1) col = getColumnCount() - 1;
    setPreferredRow(row);
    setPreferredCol(col);
    moveTo(row, col, edit, this);
  }

  public void moveUp(boolean edit) {
    int row = getSelectedRow() - 1;
    int col = getSelectedColumn();
    if(row < 0) row = 0;
    setPreferredRow(row);
    setPreferredCol(col);
    moveTo(row, col, edit, this);
  }

  public void moveDown(boolean edit) {
    int row = getSelectedRow() + 1;
    int col = getSelectedColumn();
    if(row > getRowCount() - 1) row = getRowCount();
    setPreferredRow(row);
    setPreferredCol(col);
    moveTo(row, col, edit, this);
  }

  public void paint(Graphics g) {
    //Console.logMessage(this, "paint");
    super.paint(g);
  }

  public void focusGained(FocusEvent e) {
    Component opp = e.getOppositeComponent();
    if(opp == null) {
      //Console.logMessage(this, "focusGain from (null)");
    }
    else {
      //Console.logMessage(this, "focusGain from " + opp.getClass().getName());
      _opposite = opp;
    }
    //Console.logMessage(this, "pref= " + _preferredRow + ", " + _preferredCol);

    //if(_preferredRow != -1 && _preferredCol != -1) {
    //  moveTo(_preferredRow, _preferredCol, true, this);
    //}
  }

  public void focusLost(FocusEvent e) {
    Component opp = e.getOppositeComponent();
    if(opp == null) {
      //Console.logMessage(this, "focusLoss from (null)");
    }
    else {
      //Console.logMessage(this, "focusLoss from " + opp.getClass().getName());
      _opposite = opp;
    }
  }
}
