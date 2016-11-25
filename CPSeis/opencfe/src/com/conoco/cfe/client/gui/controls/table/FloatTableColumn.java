///
/// FloatTableColumn.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-05-2002 SMCook   Constructor now takes clickCountToStart argument.
///                         Removed numerous unneeded import statements.
///

package com.conoco.cfe.client.gui.controls.table;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.text.FloatFieldDocument;
import com.conoco.cfe.client.gui.controls.text.TextFieldDocument;

/**
 * A table column that only accepts floating point data values.
 */
public class FloatTableColumn extends TextTableColumn {

  /**
   * Constructor 1 of 1.
   */
  public FloatTableColumn(int clickCountToStart, KeyListenerJTable jtable) {
    super(clickCountToStart, jtable);
  }

  /**
   * Creates the document object that will be used by the 
   * cell editor component. The document gaurantees 
   * the entry of only floating point values.
   * 
   * @return   the document that will be used by the editor
   *       component
   */
  protected TextFieldDocument createTextFieldDocument() {
    return new FloatFieldDocument();
  }
}
