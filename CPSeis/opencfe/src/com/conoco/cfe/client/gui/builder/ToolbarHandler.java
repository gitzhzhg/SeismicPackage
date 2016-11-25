// ToolbatHandler.java

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.ToolBarControl;

import java.awt.BorderLayout;

import org.w3c.dom.Node;

/** 
 * A handler for "ToolBar" node.
 */
public class ToolbarHandler extends ProcessSubTree {

  /**
   * Variable to hold the current state.
   * 
   * @serial
   */
  protected int _currentState;
  
  /**
   * Constructs a new toolbar handler.
   * 
   * @param guiState   the state object that will be acted upon by this
   *           handler
   */  
  public ToolbarHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree.
   *
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
    ToolBarControl toolBar = new ToolBarControl();
    
    _currentState = _guiState.getState();  
    _guiState.setState(GUIBuilderState.TOOLBAR_STATE);
    _guiState.setToolBar(toolBar);
    
    if ( _guiState.getTopLevelWindow() != null ) {  
      _guiState.getTopLevelWindow().add(toolBar, BorderLayout.NORTH);
    } 
    else {
      Console.logMessage("ToolbarHandler: Top Level Windos is null");
    }
  }

  /**
   * This method is invoked when the processing of the subtree
   * is complete.
   */
  public void subTreeComplete() {
    _guiState.setState( _currentState );
  }
}