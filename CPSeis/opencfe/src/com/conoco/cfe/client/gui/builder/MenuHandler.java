package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;
import com.conoco.cfe.client.gui.controls.MenuControl;

import org.w3c.dom.Node;

/**
 * A handler for the "Menu" node.
 */
public class MenuHandler extends ProcessSubTree {
  
  /**
   * Constructs a new handler.
   * 
   * @param guiState   the state object on which this handler
   *           will act
   */
  public MenuHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree
   * 
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
    String label = XMLHelper.getStringAttributeValue(node, "label");

    MenuControl menu = new MenuControl(label);
    menu.setKeyword(label);
    
    if ( _guiState.getMenuBar() != null ) {  
      _guiState.getMenuBar().add(menu);      
    } 
    else {
      Console.logMessage("MenuHandler: Menu Bar control is null");  
    }
    _guiState.registerControl(menu);
    _guiState.setMenu(menu);
  }
}