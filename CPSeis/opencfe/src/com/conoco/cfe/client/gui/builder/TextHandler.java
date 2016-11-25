// TextHandler.java

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.application.Console;

import org.w3c.dom.Node;

/**
 * A handler for "Text" node.
 */
public class TextHandler extends DontProcessSubTree {
  
  /**
   * Constructs a new handler with the specified 
   * state object.
   * 
   * @param guiState   the GUI state which will be manipulated
   *           by this handler
   */
  public TextHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree.
   *
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
    String text = XMLHelper.getCharDataString(node);
    
    if ( text == null ) {
      return;
    }
    if ( _guiState.getCurrentHelpComponent() != null ) {
      _guiState.addHelpText(
        _guiState.getCurrentHelpComponent(), text);
    } 
    else {
      //Console.logMessage("TextHandler: Couldn't find current help component");
    }
  }
}
