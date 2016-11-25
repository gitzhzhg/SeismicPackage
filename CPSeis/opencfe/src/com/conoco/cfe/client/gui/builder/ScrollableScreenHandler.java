package com.conoco.cfe.client.gui.builder;

import org.w3c.dom.Node;

import com.conoco.cfe.client.application.Console;
import com.conoco.cfe.client.gui.controls.ScreenControl;

/**
 * A handler for the "ScrollableScreen" node.
 */
public class ScrollableScreenHandler extends ProcessSubTree {
  
  /**
   * Constructs a new handler.
   * 
   * @param guiState   the state object that will be acted upon by this
   *           handler
   */
  public ScrollableScreenHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for 
   * the current part of the tree
   * 
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
    GUIBuilderState.SCROLLABLE_SCREEN = true;      
  }

  /**
   * This method is invoked when the processing of the subtree
   * is complete.
   * 
   * @param node the node to be processed
   */
  public void subTreeComplete() {
    GUIBuilderState.SCROLLABLE_SCREEN = false;
  }  
}