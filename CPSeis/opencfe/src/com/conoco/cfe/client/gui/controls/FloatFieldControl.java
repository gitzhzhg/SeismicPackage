// FloatFieldControl.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.gui.controls.text.FloatFieldDocument;
import com.conoco.cfe.client.gui.controls.text.TextFieldDocument;

/**
 * A text field control that accepts only floating point
 * values.  
 * 
 * @see com.conoco.cfe.client.gui.controls.text.FloatFieldDocument
 */
public class FloatFieldControl extends TextFieldControl {
  /**
   * Constructs a new floating point control
   */      
  public FloatFieldControl() {
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
    return new FloatFieldDocument();
  }
}