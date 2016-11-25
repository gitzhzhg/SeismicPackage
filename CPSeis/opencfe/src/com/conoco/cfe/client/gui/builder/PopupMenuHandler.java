package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.PopupMenuControl;

import org.w3c.dom.Node;

/**
 * A handler for the "PopupMenu" node.
 */
public class PopupMenuHandler extends ProcessSubTree {

  /**
   * Declares a variable for the current state.
   * 
   * @serial
   */
  protected int _currentState;
  
  /**
   * Constructs a new handler object.
   * 
   * @param guiState   the state object which is manipulated
   *           by this handler
   */
  public PopupMenuHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree
   * 
   * @param node the node to be processed
   */
  public void performAction(Node node) {
    String keyword = XMLHelper.getStringAttributeValue(node, "keyword");
    
    _currentState = _guiState.getState();
    _guiState.setState(GUIBuilderState.POPUPMENU_STATE);
    
    PopupMenuControl p = new PopupMenuControl();
    p.setKeyword(keyword);
    
    _guiState.setPopupMenu(p);
    _guiState.registerControl(p);
  }
  
  /**
   * This method is invoked when the processing of the subtree
   * is complete.
   * 
   * @param node the node to be processed
   */
  public void subTreeComplete() {
    _guiState.setState( _currentState );
  }
}