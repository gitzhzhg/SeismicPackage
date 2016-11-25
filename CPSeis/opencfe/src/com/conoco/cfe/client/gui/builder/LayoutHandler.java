package com.conoco.cfe.client.gui.builder;

import org.w3c.dom.Node;

/**
 * A handler for the "Layout" node.
 */
public class LayoutHandler extends ProcessSubTree {
  
  /**
   * Constructs a new handler.
   * 
   * @param guiState   the state object that will be acted upon by this
   *           handler
   */
  public LayoutHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for 
   * the current part of the tree
   * 
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
  }
}