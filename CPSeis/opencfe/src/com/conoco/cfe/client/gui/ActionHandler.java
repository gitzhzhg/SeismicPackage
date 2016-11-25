package com.conoco.cfe.client.gui;

import org.w3c.dom.Node;

/**
 * Interface for classes that handle nodes from GUI XML documents.
 */
public interface ActionHandler {
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree.
   * 
   * @param node the node to be processed
   */
  public void performAction(Node node);
  
  /**
   * Returns a boolean variable that indicates whether this action
   * handler handles the subtree or not.
   * 
   * @return true if ActionHandler handles the DOM sub-tree
   *         false if decoder should enumerate each sub-tree node
   */
  public boolean isHandlesSubTree();
  
  /**
   * Method gets called when all subtree processing has been completed.
   */
  public void subTreeComplete();
}