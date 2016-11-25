package com.conoco.cfe.client.gui.controls.text;

import javax.swing.text.BadLocationException;

import java.text.*;

/**
 * Class that implements the model for a text field
 * component that accepts only floating point-formatted
 * input.
 */
public class FloatFieldDocument extends TextFieldDocument {
  private DecimalFormat _format = new DecimalFormat();
    
  /**
   * Check for valid floating point characters
   * 
   * @param s the string to be formatted
   * @return   a boolean that is true if the string can 
   *       formatted as a float; false otherwise
   */
  protected boolean checkFormat(String s) {
    boolean containsDot = false;
    boolean lastWasE = false;
    for (int i=0; i < s.length(); i++) {
      char c = s.charAt(i);
      if (c == '.' && !containsDot) {
        containsDot = true;
        continue;
      }
      if (c == 'e' || c == 'E') {
        lastWasE = true;
        continue;
      }
      if (c == '+') {
        continue;
      }
      if (c== '-') {
        continue;
      }
      if ( (c < '0' || c > '9') ) {
        return false;
      }
    }
    return true;
  }
  
  /**
   */
  protected boolean format(String s) {
    try {
      Number n = _format.parse(s);
    } 
    catch ( Exception en ) {
      return false;
    }  
    return true;
  }
  
  //for testing
  public static void main(String args[]) {
    String s = new String("1.1E+12");
    FloatFieldDocument doc = new FloatFieldDocument();
    System.err.println("--->");
    System.err.println(  doc.checkFormat(s));
    System.err.println(  doc.format(s));
  }
}