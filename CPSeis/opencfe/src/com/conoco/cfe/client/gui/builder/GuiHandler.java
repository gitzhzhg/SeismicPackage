package com.conoco.cfe.client.gui.builder;

import org.w3c.dom.Node;

/**
 * The handler for the "Gui" node in the XML document
 * that describes the GUI layout. 
 */
public class GuiHandler extends ProcessSubTree {
  
  /**
   * Constructs a new handler object.
   *
   * @param guiState   the state object on which this handler
   *           will act
   */
  public GuiHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree
   * 
   * @param node the node to be processed
   */
  public void performAction(Node node) {
  }
}