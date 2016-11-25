// TipHandler.java

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import org.w3c.dom.Node;
import com.conoco.cfe.client.gui.XMLHelper;

/**
 * A handler for "Tip" node.
 */
public class TipHandler extends DontProcessSubTree {

  /**
   * Constructs a new handler with the specified 
   * state object.
   * 
   * @param guiState   the GUI state which will be manipulated
   *           by this handler
   */  
  public TipHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree.
   *
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
    String tip = XMLHelper.getCharDataString(node);

    if ( tip == null ) {
      return;  
    }
    if ( _guiState.getCurrentHelpComponent() != null ) {
      _guiState.addHelpTip(
        _guiState.getCurrentHelpComponent(), tip);
    } 
    else {
      //Console.logMessage("TextHandler: Couldn't find help component " +
      //  _guiState.getCurrentHelpComponent());
    }
  }
}
