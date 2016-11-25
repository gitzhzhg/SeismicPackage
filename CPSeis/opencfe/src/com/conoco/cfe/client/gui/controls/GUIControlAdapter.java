// GUIControlAdapter.java

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.utils.ArrayList;

import java.awt.Font;
import java.awt.Cursor;

import java.util.Hashtable;

import javax.swing.Action;

import javax.swing.SwingUtilities;

/**
 * An adapter class for GUI controls. The adapter 
 * provides default implementations for methods defined in the 
 * GUI control interface.
 * 
 * @see com.conoco.cfe.client.gui.controls.GUIControl
 */
public abstract class GUIControlAdapter implements GUIControl {
  /**
   */
  private static final String[] FUNCTION_KEYS = { "F1", "F2", "F3", 
    "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12" };

  /**
   * Declares a variable for the listeners that will be notified
   * when the control changes state
   * 
   * @serial
   */
  protected ArrayList _listeners;      
  
  /**
   */
  protected Hashtable _keyActions;
  
  /**
   * Declares a variable for storing the keyword of the control
   * 
   * @serial
   */
  protected String _keyword;
      
  /**
   * Constructs a new adapter.
   */
  protected GUIControlAdapter() {
    super();
    _listeners = new ArrayList();
    _keyActions = new Hashtable();
  }
    
  /**
   * Adds the specified GUI control listener to this control. 
   * 
   * @param l the listener that will be notified when 
   *       the GUI control changes state
   */
  public void addGUIControlListener(GUIControlListener l) {
    _listeners.add(l);  
  }
  
  /**
   */
  public String[] getFunctionKeys() {
    return FUNCTION_KEYS;
  }
  
  /**
   */
  protected Hashtable getFunctionKeyActions() {
    return _keyActions;
  }
  
  /**
   */
  protected void installFunctionKeyAction(String keyid, Action a) {
    _keyActions.put(keyid, a);
  }
  
  /**
   */
  protected Action getFunctionKeyAction(String keyid) {
    return (Action) _keyActions.get(keyid);
  } 

  /**
   * Removes the specified GUI control listener.
   * 
   * @param l the listener that needs to be removed
   */
  public void removeGUIControlListener(GUIControlListener l) {
    _listeners.remove(l);
  }
  
  /**
   * Sets the sensitivity of this control.
   * 
   * @param sensitive the boolean flag that is true if 
   *           this control is to be sensitive; false otherwise
   */
  public void setSensitive(boolean sensitive) {
  }
  
  /**
   * Sets whether this control is editable or not.
   * 
   * @param editable   the boolean flag that is true if
   *           this control is to be editable; false otherwise
   */
  public void setEditable(boolean editable) {
  }
  
  /**
   * Posts a request for obtaining focus. 
   * 
   * @param rowNumber this parameter is applicable only to array and arrayset
   *           controls; fields and other controls disregard this parameter
   */
  public void requestFocus(int rowNumber) {
  }
  
  /**
   * Sets the keyword that uniquely identifies this control.
   * 
   * @param keyword the desired keyword of this control.
   */
  public void setKeyword(String keyword) {
    _keyword = keyword.toUpperCase();  
  }
  
  /**
   * Returns the keyword of this control.
   * 
   * @return the unique keyword of this control
   */
  public String getKeyword() {
    return _keyword;
  }
 
  /**
   * Notifies the listeners that the GUI control has changed state.
   * 
   * @param e the event that is generated when this control changes
   *       state
   */
  protected void fireGUIControlChanged(GUIControlEvent e)  throws GUIControlException {
    ((GUIControl) e.getSource()).getComponent().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    
    for(int i=0; i<_listeners.size(); i++) {
      ((GUIControlListener) 
        _listeners.get(i)).guiControlChanged(e);
    }  

    ((GUIControl) e.getSource()).getComponent().setCursor(Cursor.getDefaultCursor());
  }
  
  /**
   * Sets the font on this control.
   * 
   * @param f the new font as <code>java.awt.Font</code>
   */
  public void setFont(Font f) {
  }
  
  /**
   * Disposes this control.
   */
  public void dispose() {
    _listeners.clear();
  }          
  
  /**
   */
  public void enableKeyBinding(String key) {
  }      
}
