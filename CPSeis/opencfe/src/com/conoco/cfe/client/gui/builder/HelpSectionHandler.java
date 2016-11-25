package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.gui.XMLHelper;

import org.w3c.dom.Node;

/**
 * A handler for the "HelpSection" node.
 */
public class HelpSectionHandler extends ProcessSubTree {
  
  /**
   * Constructs a new handler with the specified 
   * state object.
   * 
   * @param guiState   the GUI state which will be manipulated
   *           by this handler
   */
  public HelpSectionHandler(GUIBuilderState guiState) {
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