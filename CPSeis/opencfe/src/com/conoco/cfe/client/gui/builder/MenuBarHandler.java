package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.MenuBarControl;

import org.w3c.dom.Node;

/**
 * A handler for the "MenuBar" node.
 */
public class MenuBarHandler extends ProcessSubTree {
  
  /**
   * Declares a variable for the current state
   * 
   * @serial
   */
  protected int _currentState;
  
  /**
   * Constructs a new menubar handler.
   * 
   * @param guiState   the GUI state which will be manipulated
   *           by this handler
   */
  public MenuBarHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree
   * 
   * @param node the node to be processed
   */
  public void performAction(Node node) {
    MenuBarControl menuBar = new MenuBarControl();
    
    _currentState = _guiState.getState();  

    if ( _guiState.getTopLevelWindow() != null ) {
      _guiState.getTopLevelWindow().setMenuBar(menuBar);
    } 
    else {
      Console.logMessage("MenuBarHandler: Top Level Window is null");  
    }
    _guiState.setState(GUIBuilderState.MENU_STATE);
    _guiState.setMenuBar(menuBar);
  }

  /**
   * This method is invoked when the processing of the subtree
   * is complete.
   */
  public void subTreeComplete() {
    _guiState.setState( _currentState );
  }
}