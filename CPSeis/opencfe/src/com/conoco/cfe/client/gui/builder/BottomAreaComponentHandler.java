package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.TabPaneControl;

import java.awt.BorderLayout;

import org.w3c.dom.Node;

/**
 * A handler for the "BottomAreaComponent" node.
 */
public class BottomAreaComponentHandler extends ProcessSubTree {

  /**
   * Declares a variable for the current state.
   * 
   * @serial
   */
  protected int _currentState;
  
  /**
   * Constructs a new handler.
   * 
   * @param guiState   the state object that will be acted upon by this
   *           handler
   */
  public BottomAreaComponentHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the current 
   * part of the tree.
   * 
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
    _currentState = _guiState.getState();    
    _guiState.setState(GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE);
    if ( _guiState.getTopLevelWindow() != null ) {
      _guiState.getTopLevelWindow().setBottomAreaComponentPresent(true);
    } 
    else {
      Console.logMessage("BottomAreaComponentHandler: Top Level Window is null");
    }
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