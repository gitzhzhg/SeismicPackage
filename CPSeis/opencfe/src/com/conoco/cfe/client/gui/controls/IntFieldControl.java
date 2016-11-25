/// IntFieldControl.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-05-2002 SMCook   Corrected source file name.


package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.gui.controls.text.IntegerFieldDocument;
import com.conoco.cfe.client.gui.controls.text.TextFieldDocument;

/**
 * A text field control that accepts only integer values.
 * 
 * @see com.conoco.cfe.client.gui.controls.text.IntegerFieldDocument
 */
public class IntFieldControl extends TextFieldControl {
  /**
   * Constructs a new integer control
   */      
  public IntFieldControl() {
    super();
  }

  /**
   * Creates the document that will be used by the control.
   * The document has the responsibility of checking the 
   * format of the string entered in the field.
   * 
   * @return the document that will be used for the field control
   */  
  protected TextFieldDocument createDocument() {
    return new IntegerFieldDocument();
  }
}
