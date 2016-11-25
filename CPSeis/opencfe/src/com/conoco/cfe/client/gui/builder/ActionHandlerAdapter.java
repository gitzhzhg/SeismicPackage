package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.gui.ActionHandler;

import org.w3c.dom.Node;

/**
 * An adapter class that can be extended to create new
 * actions. These actions correspond to the different
 * GUI elements and attributes in the document that 
 * describes the GUI layout.
 */
public abstract class ActionHandlerAdapter implements ActionHandler {
  
  /**
   * Declares a variable for the gui builder state.
   * 
   * @serial
   */
  protected GUIBuilderState _guiState;

  /**
   * Constructs a new adapter using the state object specified.
   * 
   * @param guiState   the state object that will be acted upon by this
   *           handler
   */
  public ActionHandlerAdapter(GUIBuilderState guiState) {
    _guiState = guiState;
  }
  
  /**
   * Passes the ActionHandler the DOM Tree node for the current part 
   * of the tree for processing. This method should be implemented
   * by subclasses.
   * 
   * @param node the node in the document to be processed
   */
  public abstract void performAction(Node node);
  
  /**
   * Indicates whether the handler processes the subtree or not.
   * 
   * @return   <code>true</code> if the handler processes subtree;
   *       <code>false</code> otherwise
   */
  public abstract boolean isHandlesSubTree();
  
  /**
   * This method is invoked when the processing of the subtree
   * is complete.
   */
  public void subTreeComplete() {
  }
}