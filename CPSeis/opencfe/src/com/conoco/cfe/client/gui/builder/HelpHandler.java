// HelpHandler.java

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.XMLHelper;

import org.w3c.dom.Node;

/**
 * A handler for the "Help" node.
 */
public class HelpHandler extends ProcessSubTree {
  
  /**
   * Creates a new handler.
   * 
   * @param guiState   the state object that is manipulated 
   *           ny this handler
   */
  public HelpHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree
   * 
   * @param node the node to be processed
   */
  public void performAction(Node node) {
    String component = XMLHelper.getStringAttributeValue(node, "component");      
    GUIControl control = _guiState.getControl(component.toUpperCase());
    
    if ( _guiState.getState() == GUIBuilderState.COMMAND_STATE ) { 
      if ( control != null ) {    
        _guiState.setCurrentHelpComponent( control.getKeyword());
      } 
      else {
        //Console.logMessage("HelpHandler: Couldn't find control " + component);  
      }
    } 
    
    else {
      // process help element    
      _guiState.setCurrentHelpComponent(control.getKeyword());
    }    
  }
}
