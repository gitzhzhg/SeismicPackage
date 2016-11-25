// DontProcessSubTree.java

package com.conoco.cfe.client.gui.builder;

import org.w3c.dom.Node;

/**
 * An XML document node handler which handles subtree
 * processing.
 */
public abstract class DontProcessSubTree extends ActionHandlerAdapter {
  
  /**
   * Constructs a new handler object.
   * 
   * @param guiState   the state object on which this handler
   *           will act
   */
  public DontProcessSubTree(GUIBuilderState state) {
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
   * @return   a boolean variable that is <code>true</code> since
   *       this handler handles its subtree
   */
  public boolean isHandlesSubTree() {
    return true;
  }
}