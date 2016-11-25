// SeparatorHandler.java

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.SeparatorControl;

import javax.swing.SwingConstants;

import org.w3c.dom.Node;

/**
 * A handler for the "Separator" node.
 */
public class SeparatorHandler extends ProcessSubTree {
  
  /**
   * Constructs a new handler.
   * 
   * @param guiState   the state object on which this handler
   *           will act
   */
  public SeparatorHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree
   * 
   * @param node the node to be processed
   */
  public void performAction(Node node) {
    SeparatorControl sc = new SeparatorControl(SwingConstants.HORIZONTAL);
    _guiState.registerControl(sc);
    
    if ( _guiState.getState() == GUIBuilderState.MENU_STATE ) {
      if ( _guiState.getMenu() != null ) {
        _guiState.getMenu().add(sc);
      } 
      else {
        Console.logMessage("SeparatorHandler: Menu Control is null");
      }  
    } 
    
    else if ( _guiState.getState() == GUIBuilderState.POPUPMENU_STATE) {
      if ( _guiState.getPopupMenu() == null ) {
        _guiState.getPopupMenu().add(sc);  
      } 
      else {
        Console.logMessage("SeparatorHandler: Popup Menu Control is null");
      }
    } 
    
    else if ( _guiState.getState() == GUIBuilderState.TOOLBAR_STATE) {
      if ( _guiState.getToolBar() != null ) {
        _guiState.getToolBar().add(sc);    
      } 
      else {
        Console.logMessage("SeparatorHandler: Toolbar Control is null");
      }
    }
  }
}