///
/// TextFieldDocument.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  6.
///  5. 08-20-2002 SMCook   Implemented workaround for apparent Java source
///                          bug, exchanging
///                           javax.swing.text.PlainDocument for
///                          com.conoco.cfe.workarounds.
///                           javax.swing.text.PlainDocument.
///                         The hang was occurring at the insertString()
///                          function.  Deeper in the source, it was found that
///                          the actual hang was occurring in function
///                          preferenceChanged() in Java source code file
///                          View.java.
///  4. 08-13-2002 SMCook   Documentation change only.  Marked the line of code
///                          containing the insertString() call that is hanging
///                          the GUI upon the second entry into the dialog
///                          that allows the user to change the directory.
///                          Appears to be a bug in the Java VM.
///

package com.conoco.cfe.client.gui.controls.text;

import java.awt.Toolkit;

import javax.swing.text.AttributeSet;
//import javax.swing.text.PlainDocument;
import com.conoco.cfe.workarounds.javax.swing.text.PlainDocument;
import javax.swing.text.BadLocationException;


/**
 * A class to implement model of the text field editor component
 * that comes into play during the editing of array components. 
 * It is the model's responsibility to check whether the input
 * is valid or not.
 */
public class TextFieldDocument extends PlainDocument {
  /**
   * Variable to store the maximum length of the 
   * input that can be entered in the editor 
   * component
   * 
   * @serial
   */  
  protected int     _maxLength=0;
  
  /**
   * A boolean variable that is set if this variable is 
   * modified; false otherwise
   * 
   * @serial
   */
  protected boolean   _modified = false;
  
  /**
   */
  protected boolean _bypassFormatCheck = false;
  
  /**
   * Indicates whether this document has been modified or not.
   * 
   * @return   <code>true</code> if it has been modified;
   *       <code>false</code> otherwise
   */
  public boolean isModified() {
    return _modified;
  }
  
  /**
   * Sets the boolean flag that will indicate whether this
   * document has been modified or not.
   *
   * @param modified the boolean value
   */
  public void setModified(boolean modified) {
    _modified = modified;
  }
  
  /**
   * Returns the maximum length of the input that this 
   * document can accept.
   * 
   * @return the maximum number of characters
   */
  public int getMaxLength() {
    return _maxLength;
  }
  
  /**
   * Sets the maximum length of the input that can be 
   * entered in this document.
   * 
   * @param columns the maximum length
   */
  public void setMaxLength(int columns) {
    _maxLength = columns;
  }
  
  /**
   * Checks to see if the maximum length of the characters
   * is being exceeded or not.
   * 
   * @return   a boolean that is false if length is being exceeded;
   *       true otherwise
   */
  protected boolean checkLength() {
    if (getLength()+1 > _maxLength ) {
      Toolkit.getDefaultToolkit().beep();
      return false;  
    }
    return true;
  }
  
  /**
   * Checks the format of the entered string.
   * For plain text field format is always true. 
   * 
   * @return   a boolean that is true if format is valid;
   *       false otherwise
   */
  protected boolean checkFormat(String s) {  
    return true;
  }
  
  /**
   * Inserts a string with the given attribute set and at the given offset.
   *
   * @param offs              the offset
   * @param str           the string to be inserted
   * @param a           the attribute set
   */
  public void insertString(int offs, String str, AttributeSet a)
                    throws BadLocationException {
    if (!checkLength()) {
      System.out.println("BadLocation::checkLength");
      System.err.println("ERROR trying to insert: " + str);
      throw new BadLocationException(str, offs);
    }
    if (str.length() > getMaxLength()) {
      System.out.println("BadLocation::length > maxlength");
      System.err.println("ERROR trying to insert: " + str);
      throw new BadLocationException(str, offs);
    }
    if (!checkFormat(str)) {
      throw new BadLocationException(str, offs);
    }
    _modified = true;
    super.insertString(offs, str, a);     //SMCook 1.4 hangs here
  }

  /**
   * Removes a part of the string.
   *
   * @param offs              the offset
   * @param len           the length of the input to be removed
   */
  public void remove(int offs, int len) throws BadLocationException {
    super.remove(offs, len);
    _modified = true;
  }
}
