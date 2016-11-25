// IntegerFieldDocument.java

package com.conoco.cfe.client.gui.controls.text;

/**
 * Class that implements the model for a text field
 * component that accepts only integer-formatted
 * input.
 */
public class IntegerFieldDocument extends TextFieldDocument {
  /**
   * Check for valid integer characters.
   * 
   * @param s the string to be formatted
   * @return   a boolean that is true if the string can 
   *       formatted as an integer; false otherwise
   */
  protected boolean checkFormat(String s) {
    for (int i=0; i < s.length(); i++) {
      char c = s.charAt(i);
      if (c == '-' && getLength() == 0) {
        continue;
      }
      if (c < '0' || c > '9') {
        return false;
      }
    }
    return true;
  }
}