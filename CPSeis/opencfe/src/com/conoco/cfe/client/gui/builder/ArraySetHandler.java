package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.ArraySetControl;
import com.conoco.cfe.client.gui.controls.ScreenControl;

import org.w3c.dom.Node;

/**
 * A handler for handling array sets.
 */
public class ArraySetHandler extends ProcessSubTree {
  
  /**
   * Variable for storing the the gui builder state
   * before this handler performs itc action.
   */
  protected int _currentState;
  
  /**
   * Constructs a new arrayset handler.
   * 
   * @param guiState   the state object that will be acted upon by this
   *           handler
   */
  public ArraySetHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Passes the ActionHandler the DOM Tree node for the current part 
   * of the tree for processing. This method should be implemented
   * by subclasses.
   * 
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
    String keyword     = XMLHelper.getStringAttributeValue(node, "keyword");
    String label     = XMLHelper.getStringAttributeValue(node, "label");
    boolean sensitive    = XMLHelper.getBooleanAttributeValue(node, "sensitive");
    String clickMode     = XMLHelper.getStringAttributeValue(node, "clickMode");
    
    if ( label == null ) {
      label = keyword;
    }
  
    _currentState = _guiState.getState();
    _guiState.setState( GUIBuilderState.ARRAYSET_STATE);
    ArraySetControl asc = new ArraySetControl();
    asc.setSensitive(sensitive);
    if ( clickMode.equalsIgnoreCase("ignore") ) {
      asc.setClickModeImmediate(false);        
    } 
    else {
      asc.setClickModeImmediate(true);        
    }
    asc.setKeyword(keyword);
    _guiState.registerControl(asc);
    _guiState.setArraySet(asc);    
  }
  
  /**
   * This method is invoked when the processing of the subtree
   * is complete.
   */
  public void subTreeComplete() {
    ScreenControl sc = _guiState.getScreen();
    if ( sc != null ) {
      sc.add(  _guiState.getArraySet());
    } 
    else {
      Console.logMessage("ArraySetHandler: Screen control is null");  
    }
    _guiState.setState(_currentState);
  }  
}