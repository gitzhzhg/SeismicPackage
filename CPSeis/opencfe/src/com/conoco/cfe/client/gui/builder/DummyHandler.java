package com.conoco.cfe.client.gui.builder;

import org.w3c.dom.Node;

/**
 * 
 */
public class DummyHandler extends ProcessSubTree {
  
  public DummyHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the current part of the tree
   */
  public void performAction(Node node) {
  }
  
}