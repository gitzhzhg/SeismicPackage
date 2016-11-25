///
/// TextTableColumn.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  6.
///  5. 09-05-2002 SMCook   Constructor now takes clickCountToStart and jtable
///                          arguments.
///                         KeyListenerJTextField is now used as argument to
///                          DefaultCellEditor instead of JTextField.
///                         Removed obsolete inner class myCellEditor.
///  4. 08-21-2002 SMCook   Commented out main impediment to cut and paste -
///                          the mandatory text-grab logic within myCellEditor.
///  3. 09-18-2001 SMCook   Removed debug statement, corrected cosmetics/typos.
///


package com.conoco.cfe.client.gui.controls.table;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.gui.controls.text.TextFieldDocument;

import java.awt.Color;
import java.awt.Component;

import java.awt.event.MouseEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;

import java.util.EventObject;

import javax.swing.KeyStroke;
import javax.swing.JComponent;

import javax.swing.DefaultCellEditor;
import javax.swing.JTextField;
import javax.swing.JComboBox;
import javax.swing.BorderFactory;

import javax.swing.table.TableColumn;
import javax.swing.table.TableCellEditor;

import javax.swing.text.Keymap;

/**
 * Implements the functionality of a table column for array 
 * and an arrayset components. 
 */
class TextTableColumn extends TableColumn {
  /**
   * Declares the variable for the editor component. This 
   * component is basically a text field.
   * 
   * @serial
   */
  protected JTextField _textField;
  
  /**
   * Declares a variable for the document object that is used 
   * by the text field component
   * 
   * @serial
   */
  protected TextFieldDocument _textFieldDocument;
  
  /**  
   * A variable for the listener object that gets notified 
   * when the component gains focus
   *
   * @serial
   */ 
  protected FocusListener _focusListener;

  /**
   * Constructor when editor is a JTextField
   */
  public TextTableColumn(int clickCountToStart, KeyListenerJTable jtable) {
    super();
    _textFieldDocument = createTextFieldDocument();
    _focusListener = new FocusWatcher();
    _textField = new KeyListenerJTextField(jtable);
    
    _textField.setFont(ClientConstants.getDefaultFieldFont());
    _textField.addFocusListener(_focusListener);
    
    //register enter key
    KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
    Keymap map = _textField.getKeymap();
    map.removeKeyStrokeBinding(enter);    
    _textField.registerKeyboardAction( new ReturnAction(), "Return",
                      KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0),
                        JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    _textField.setDocument(_textFieldDocument);
    _textField.setBorder(BorderFactory.createLineBorder(Color.red, 2));

    //Editor is a Text Field
    DefaultCellEditor cellEditor = new DefaultCellEditor(_textField);
    cellEditor.setClickCountToStart(clickCountToStart);
    setCellEditor(cellEditor);

    setHeaderRenderer(new ConocoTableHeaderRenderer());
  }
  
  /**
   * Constructor when editor is a JComboBox
   */
  public TextTableColumn(String keyword) {
    super();
    _textFieldDocument = createTextFieldDocument();
    _textField = new JTextField(); 
    JComboBox combo = new JComboBox();    
    DefaultCellEditor cellEditor = new DefaultCellEditor(combo);
    cellEditor.setClickCountToStart(1);
    setCellEditor(cellEditor);    
    setHeaderRenderer(new ConocoTableHeaderRenderer());
  }

  /**
   * Creates the document object that will be used by the 
   * editor component.
   * 
   * @return   the document object that will be used by the 
   *       text field editor component
   */
  protected TextFieldDocument createTextFieldDocument() {
    return new TextFieldDocument();
  }

  /**
   * Posts a request for obtaining focus. 
   * 
   * @param rowNumber this parameter is applicable only to array and arrayset
   *           controls; fields and other controls disregard this parameter
   */
  public void requestFocus(int rowNumber) {
    _textField.requestFocus();  
  }
  
  /**
   * Sets the maximum number of characters that can be entered 
   * in the editor component.
   * 
   * @param size the maximum number of characters
   */
  public void setMaxLength(int size) {
    _textFieldDocument.setMaxLength(size);
    _textField.setColumns(size);    
  }
  
  /**
   * Need this as the JTable wont lay itself out properly
   */
  public int getPreferredWidth() {
    return getWidth();
  }
  
  /**
   * Returns the maximum length of characters that can be 
   * entered in the editor text field component.
   * 
   * @return the maximum length
   */
  public int getMaxLength() {
    return _textFieldDocument.getMaxLength();
  }
  
  /**
   * Returns the text field component.
   * 
   * @return the text field component
   */
  public JTextField getTextField() {
    return _textField;
  }
  
  /**
   * Returns the class of the object that can be inserted 
   * in the column. All objects that are inserted are 
   * strings.
   * 
   * @return the class of the objects that are inserted
   */        
  public Class getColumnClass() {
    return String.class;
  }
  
  /**
   * Returns the string representation of this object.
   * 
   * @return the string representation
   */    
  public String toString() {
    return "TextTableColumn";
  }
  
  /**
   * Inner class that sends focus-related notifications to the
   * GUI control listeners.
   */
  class FocusWatcher implements FocusListener {
    /**
     * This method is invoked when focus is gained by this 
     * control
     * 
     * @param e the event that is generated when focus
     *       is gained by this control
     */
    public void focusGained(FocusEvent e) {
    }

    /**
     * This method is invoked when focus is lost by this 
     * control. Stop the editing when quit the cell.
     * 
     * @param e the event that is generated when focus
     *       is lost by this control
     */    
    public void focusLost(FocusEvent e) {
      //getCellEditor().stopCellEditing();  //SMCook commented
    }
  }  
  
  /**
   * Inner class that implements the "Return" key action on this text field.
   */
  protected class ReturnAction implements ActionListener {
    public void actionPerformed(ActionEvent e) {  
      ((DefaultCellEditor) getCellEditor()).stopCellEditing();
    }
  } 
}

/*
  SMCook - obsolete inner class - interferes with Cut/Paste in 1.4

//implemented to be in replace mode when a cell is selected.
//previous string in the cell is automatically hightlighted.

class myCellEditor extends DefaultCellEditor {
  JTextField _tf;

  public myCellEditor(JTextField tf) {
    super(tf);
    _tf = tf;
    setClickCountToStart(2);
  }
  public boolean shouldSelectCell(EventObject anEvent) {
    EventObject _anEvent;
    _anEvent = anEvent;
    if (_anEvent instanceof KeyEvent){
      KeyEvent _ke;
      _ke = (KeyEvent) _anEvent;
      _tf.requestFocus();
      _tf.selectAll();
      _tf.copy();
    }
    if (_anEvent instanceof MouseEvent){
      _tf.selectAll();
      _tf.copy();
    } 
    return true;
  }
}

*/
