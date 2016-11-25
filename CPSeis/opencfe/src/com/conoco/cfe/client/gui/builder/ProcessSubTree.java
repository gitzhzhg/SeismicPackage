// ProcessSubTree.java

package com.conoco.cfe.client.gui.builder;

import org.w3c.dom.Node;

/**
 * A DOM tree node handler that does not handle 
 * the node sub tree. 
 */
abstract class ProcessSubTree extends ActionHandlerAdapter {
  
  /**
   * Constructs a new handler.
   * 
   * @param state   the GUI state which will be manipulated
   *           by this handler
   */
  public ProcessSubTree(GUIBuilderState state) {
    super(state);
  }
  
  /**
   * This is an abstract method which subclasses should override.
   * 
   * @param node the document node to be processed
   */
  public abstract void performAction(Node node);

  /**
   * This method indicates whether this handler handles subtree
   * or not.
   * 
   * @return   a boolean variable that is <code>false</code> since
   *       this handler does not handle its subtree
   */  
  public boolean isHandlesSubTree() {
    return false;
  }
}