package com.conoco.cfe.client.gui.builder;

import javax.swing.ButtonGroup;

import org.w3c.dom.Node;

/**
 * A handler for "RadioButton" node.
 */
public class RadioButtonHandler extends ProcessSubTree {

  /**
   * Declares a variable for the current state.
   * 
   * @serial
   */
  protected int _currentState;
  
  /**
   * Constructs a new handler.
   * 
   * @param guiState   the state object on which this handler
   *           will act
   */
  public RadioButtonHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree.
   *
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
    _currentState = _guiState.getState();
    _guiState.setState( GUIBuilderState.RADIOBUTTON_STATE);
    ButtonGroup grp = new ButtonGroup();
    _guiState.setRadioButtonGroup(grp);
  }
  
  /**
   * This method is invoked when the processing of the subtree
   * is complete.
   */
  public void subTreeComplete() {
    _guiState.setState( _currentState );
  }
}