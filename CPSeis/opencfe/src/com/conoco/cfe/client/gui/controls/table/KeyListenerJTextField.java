///
/// KeyListenerJTextField.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  3.
///  2. 09-27-2002 SMCook   Now also call clearSelection() when editing is
///                          stopped (one-click arrays only).
///  1. 09-05-2002 SMCook   Original version.  Created for the purpose of
///                          having clearer editing functionality in arrays.
///

package com.conoco.cfe.client.gui.controls.table;

import com.conoco.cfe.client.application.Console;

import java.awt.Component;
import java.awt.Font;
import java.awt.Point;
import java.awt.Rectangle;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JTextField;

public class KeyListenerJTextField
  extends JTextField implements FocusListener, KeyListener, MouseListener {

  private final KeyListenerJTable _jtable;

  private int _pageUpCount;

  private boolean _hasMoved;

  /**
   * Constructor 1 of 1.
   */
  public KeyListenerJTextField(KeyListenerJTable jtable) {
    _jtable = jtable;

    addFocusListener(this);
    addKeyListener(this);
    addMouseListener(this);
  }

  public void setFont(Font f) {
    Font tmp = new Font(f.getName(), f.BOLD, f.getSize());
    super.setFont(tmp);
  }

  public void setCaretPosition(int pos) {
    super.setCaretPosition(pos);
  }

  /**
   * Logic for when key is pressed.
   */
  public void keyPressed(KeyEvent e) {

    Object src = e.getSource();
    if(src != this) {
      //Console.logMessage(this, "src was " + src.getClass().getName());
      return;
    }
    int code = e.getKeyCode();
    int caretPosition = getCaretPosition();

    boolean isModified = false;
    if(e.getModifiers() != 0) isModified = true;

// VK_PAGE_UP
    if(code == e.VK_PAGE_UP) {
      if(_pageUpCount == 0) {
        int icol = _jtable.getSelectedColumn();
        moveTo(0, icol, true);
        if(icol != 0) _pageUpCount++;
      }
      else {
        moveTo(0, 0, true);
        _pageUpCount = 0;
      }
      e.consume();
      return;
    }

// VK_ESCAPE
    else if(code == e.VK_ESCAPE) {
      moveTo(0, _jtable.getSelectedColumn(), true);
      e.consume();
      return;
    }

// VK_TAB
    else if(code == e.VK_TAB) {
      if(isModified)
        moveLeft(true);
      else 
        moveRight(true);

      e.consume();
      return;
    }

// VK_LEFT
    else if(code == e.VK_LEFT) {
      if(isModified ||
         (caretPosition <= 0 && _jtable.getSelectedColumn() > 0)) {
        moveLeft(true);
      e.consume();
      return;
      }
    }

// VK_RIGHT
    else if(code == e.VK_RIGHT) {
      if(isModified || caretPosition >= getText().length()) {
        moveRight(true);
      e.consume();
      return;
      }
    }

// VK_UP
    else if(code == e.VK_UP) {
      moveUp(true);
      e.consume();
      return;
    }

// VK_DOWN
    else if(code == e.VK_DOWN) {
      moveDown(true);
      e.consume();
      return;
    }

// VK_ENTER
    else if(code == e.VK_ENTER) {
      moveDown(true);
      e.consume();
      return;
    }
  }

  private void moveTo(int row, int col, boolean edit) {
    _hasMoved = false;
    _jtable.moveTo(row, col, edit, this);
  }

  private void moveLeft(boolean edit) {
    _hasMoved = true;
    _jtable.moveLeft(edit);
  }

  private void moveRight(boolean edit) {
    _hasMoved = true;
    _jtable.moveRight(edit);
  }

  private void moveUp(boolean edit) {
    _hasMoved = true;
    _jtable.moveUp(edit);
  }

  private void moveDown(boolean edit) {
    _hasMoved = true;

    if((_jtable.getSelectedRow() == _jtable.getRowCount() - 1) &&
        getText().equals("")                 ) return;

    _jtable.moveDown(edit);
  }

  public void keyReleased(KeyEvent e) {
    // no action
  }

  public void keyTyped(KeyEvent e) {
    // no action
  }

  /**
   * Need to return true for isManagingFocus().  Otherwise, VK_TAB events
   * do not arrive.
   */
  public boolean isManagingFocus() {
    return true;
  }

  /**
   * Logic for focus.
   */
  public void focusGained(FocusEvent e) {
    //Component opp = e.getOppositeComponent();
    //if(opp != null)
    //  Console.logMessage(this, "focusGain " + opp.getClass().getName());
    //else
    //  Console.logMessage(this, "focusGain from (null)");

    invalidate();

    Rectangle r = getBounds();
    r.y -= r.height;                 // makes adjacent cells visible also
    r.height *= 3;                   //
    _jtable.scrollRectToVisible(r);

    Point pt = getLocation();
    _jtable.setSelectedRow(_jtable.rowAtPoint(pt));
    _jtable.setSelectedColumn(_jtable.columnAtPoint(pt));

    _jtable.scrollRectToVisible(r);  // once more for good measure

    if(_hasMoved) setCaretPosition(getText().length());
  }

  public void focusLost(FocusEvent e) {
    Component opp = e.getOppositeComponent();
    //if(opp != null)
    //  Console.logMessage(this, "focusLoss from " + opp.getClass().getName());
    //else
    //  Console.logMessage(this, "focusLoss from (null)");

    _hasMoved = false;

    if(opp == _jtable) {
      return;
    }
    else {
      _jtable.clearSelection();
      _jtable.stopCellEditing();
    }
  }

  /**
   * Mouse listener logic (helps with caret).
   */
  public void mousePressed(MouseEvent e) {
    _hasMoved = false;
    Point pt = getLocation();
    pt.x += e.getX();
    pt.y += e.getY();
    int row, col;
    _jtable.setPreferredRow(row = _jtable.rowAtPoint(pt));
    _jtable.setPreferredCol(col = _jtable.columnAtPoint(pt));
    moveTo(row, col, false);
  }
  public void mouseReleased(MouseEvent e) {
  }
  public void mouseClicked(MouseEvent e) {
  }
  public void mouseEntered(MouseEvent e) {
  }
  public void mouseExited(MouseEvent e) {
  }
}
