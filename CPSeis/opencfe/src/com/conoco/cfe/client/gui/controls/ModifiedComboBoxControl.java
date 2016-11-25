// ModifiedComboBoxControl.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.utils.TreeSet;

import java.awt.Component;
import java.awt.Font;
import java.awt.BorderLayout;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyListener;

import javax.swing.KeyStroke;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.ListSelectionModel;
import javax.swing.JTextField;
import javax.swing.JScrollPane;
import javax.swing.JPanel;
import javax.swing.JFrame;

import javax.swing.event.DocumentListener;
import javax.swing.event.DocumentEvent;

import javax.swing.text.Document;
import javax.swing.text.Keymap;

/**
 * A special control consisting of a text field and a 
 * scrollable list of items. A proper item is selected 
 * in the list depending upon the input in the text field.
 */
public class ModifiedComboBoxControl extends GUIControlAdapter 
  implements FieldGUIControl {
  /**
   * Static initializer to remove the default 'enter' key binding.
   */
  static {
    JTextField f = new JTextField();
    KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
    Keymap map = f.getKeymap();
    map.removeKeyStrokeBinding(enter);
  }  
  
  /**
   * Variable for the list
   * 
   * @serial
   */
  protected JList _list;
  
  /**
   * Variable for the field 
   * 
   * @serial
   */
  protected JTextField _field;
  
  /**
   * Variable for the scroll pane
   * 
   * @serial
   */
  protected JScrollPane _scroll;
  
  /**
   * Variable for the document listener
   * 
   * @serial
   */
  protected DocumentListener _docListener;

  /** 
   * Variable for key listener
   * 
   * @serial
   */
  protected KeyListener _keyListener;
  
  /**
   * Variable for the panel component that contains the
   * field and the scroll pane
   * 
   * @serial
   */
  protected JPanel _panel;
  
  /**
   * Constructs a new modified combo box control.
   */  
  public ModifiedComboBoxControl() {
    super();  
    _field = new JTextField();
    _field.registerKeyboardAction( new ReturnAction(), "Return",
                              KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0),
                              JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    _list = new JList();
    _list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    _scroll = new JScrollPane(_list);
    _docListener = new MyDocumentListener();
    _field.getDocument().addDocumentListener(_docListener);
    _panel = new JPanel(new BorderLayout());
    _panel.add(_field, BorderLayout.NORTH);
    _panel.add(_scroll, BorderLayout.CENTER);
  }
    
  /**
   * Sets the items on the list component of this control.
   * 
   * @serial
   */
  public void setItems(String[] items) {
    TreeSet set = new TreeSet();
    for ( int i = 0; i < items.length; i++ ) {
      set.add(items[i]);  
    }
    _list.setListData(set.toArray());
  }
  
  /**
   * Sets the font on this control.
   * 
   * @param f the font to be set on this control
   */
  public void setFont(Font f) {
    _field.setFont(f);
    _list.setFont(f);
  }
  
  /**
   * Sets the sensitivity of this control.
   * 
   * @param sensitive the boolean flag that is true if 
   *           this control is to be sensitive; false otherwise
   */
  public void setSensitive(boolean b) {
    _field.setEnabled(b);
    _list.setEnabled(b);  
  }
  
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _panel;
  }
  
  /**
   * This methof appears in this control as part of the 
   * implementation of the <code>com.conoco.cfe.client.gui.controls.FieldGUIControl</code>.
   * This method is irrelevant for this control.
   */
  public void setValue(String val) {
  }
  
  /**
   * Disposes this control.
   */
  public void dispose() {
    _field.getDocument().removeDocumentListener(_docListener);
    super.dispose();
  }                
  
  /**
   * Inner class that is used to set up a listener that gets called
   * when the content of the field changes.
   */
  private class MyDocumentListener implements DocumentListener {
    /**
     * Gives notification that an attribute or set of attributes changed.  
     */
    public void changedUpdate(DocumentEvent e) {
      Document d = e.getDocument();
      try {
        process(d.getText(0, d.getLength()));
      } 
      catch ( Exception ex ) {}        
    }
    
    /**
     * Gives notification that there was an insert into the document. 
     * The given range bounds the freshly inserted region.    
     */
    public void insertUpdate(DocumentEvent e) {
      Document d = e.getDocument();
      try {
        process(d.getText(0, d.getLength()));      
      } 
      catch ( Exception ex ) {}
    }
    
    /**
     * Gives notification that a portion of the document has been removed. 
     * The range is given in terms of what the view last saw
     * (that is, before updating sticky positions).    
     */
    public void removeUpdate(DocumentEvent e) {
      Document d = e.getDocument();
      try {
        process(d.getText(0, d.getLength()));      
      } 
      catch ( Exception ex ) {}
    }
    
    /**
     * Processes the string that has been entered in the field.
     * This method reads the string input and selects the 
     * entry in the array that matches the input.
     * 
     * @param s the input string
     */
    private void process(String s) {
      int counter = s.length();
      int index = -1;
      while ( (index = containsString(s)) == -1 ) {
        s = s.substring(0, counter);
        --counter;  
      }
      select(index);
    }
    
    /**
     * Selects the specified index in the list of items and
     * moves the scrollable lists's viewport so that the selected
     * item is at the top of the view.
     * 
     * @param index the index to be selected
     */
    private void select(int index) {
      _list.setSelectedIndex(index);
      _scroll.getViewport().setViewPosition(
      _list.indexToLocation(index));
    }
    
    /**
     * Checks to see if the specified string matches 
     * any string in the array ( either partly or fully ).
     * 
     * @param s the string to be checked for
     * @return   the index of the element that matched either
     *       partly or fully with the specified string; 
     *       -1 if no match is found
     */
    private int containsString(String s) {
      int listLength = _list.getModel().getSize();
      for ( int i = 0; i < listLength; i++ ) {
        String current = (String) _list.getModel().getElementAt(i);
        if ( s.regionMatches(true, 0, current, 0, s.length()) ) {
          return i;
        }    
      }
      return -1;
    }
  }
  
  /**
   * Inner class that implements the "Return" key action on this text field.
   */
  protected class ReturnAction implements ActionListener {
    public void actionPerformed(ActionEvent e) {  
      GUIControlEvent event = new 
        GUIControlEvent(ModifiedComboBoxControl.this, 
          GUIControlEvent.MODIFY_FIELD_EVENT,
            _field.getText());
      try {
        fireGUIControlChanged(event);
      } 
      catch (GUIControlException en) {}    
    }
  } 
  
  // for testing
  public static void main(String args[]) {
    JFrame f = new JFrame();        
    ModifiedComboBoxControl c = new ModifiedComboBoxControl();
    c.addGUIControlListener( new ControlListener());
    String[] els = {"One", "Two", "Three", "Four", "Five", "Six",
                    "Seven", "Eight", "Nine", "Ten" };
    c.setItems(els);
    f.getContentPane().add(c.getComponent(), BorderLayout.CENTER);
    f.setSize(200,200);
    f.setVisible(true);
  }
  
  // for testing only
  static class ControlListener implements GUIControlListener {
    public void guiControlChanged(GUIControlEvent e) {
      System.out.println("*****"  + e.getValue());
    }  
  }
}