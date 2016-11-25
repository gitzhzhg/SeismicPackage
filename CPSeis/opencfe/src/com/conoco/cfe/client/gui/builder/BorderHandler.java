// BorderHandler.java

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.GUIControlAdapter;
import com.conoco.cfe.client.gui.controls.BorderPanelControl;
import com.conoco.cfe.client.gui.controls.WindowControl;

import com.conoco.cfe.utils.ArrayList;

import java.awt.Insets;
import java.awt.Component;
import java.awt.Container;
import java.awt.Color;

import java.util.Hashtable;

import javax.swing.JPanel;
import javax.swing.BorderFactory;

import org.w3c.dom.Node;

/**
 * A handler for the "Border" node in the document that describes
 * the GUI layout.
 */
public class BorderHandler extends ProcessSubTree {

  /**
   * Variable for the current state the GUI state object
   * is in
   * 
   * @serial
   */
  protected int _currentState;
    
  /**
   * Constructs a new handler object.
   * 
   * @param guiState   the state object which is manipulated
   *           by this handler
   */
  public BorderHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the current part of the tree.
   * 
   * @param node the DOM tree node which is to be processed
   */
  public void performAction(Node node) {
    Hashtable attrs = XMLHelper.getAttributesAsHashtable(node);
    Object title = attrs.get("title");
    Object keyword = attrs.get("keyword");
        
    BorderPanelControl panel = new BorderPanelControl();
    panel.setOpaque(false);
    panel.setKeyword( (String) keyword);
    
    if ( title == null ) {
      panel.setBorder( 
        BorderFactory.createLineBorder(Color.gray, 1));  
    } 
    else {
      panel.setBorder(
        BorderFactory.createTitledBorder((String) title));  
    }
    
    _guiState.registerControl(panel);
    
    if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {  
      if ( _guiState.getScreen() != null ) {
        _guiState.getScreen().add(panel);
      } 
      else {
        Console.logMessage("BorderHandler: Screen control is null");  
      }
    } 
    
    else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
      WindowControl wc = _guiState.getTopLevelWindow();
      if ( wc != null ) {
        JPanel p = wc.getBottomComponentPanel();
        p.add(panel.getComponent());
      } 
      else {
        Console.logMessage("BorderHandler: Top Level Window is null");
      }
    } 
    
    else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
      WindowControl wc = _guiState.getTopLevelWindow();
      if ( wc != null ) {
        JPanel p = wc.getTopComponentPanel();
        p.add(panel.getComponent());
      } 
      else {
        Console.logMessage("BorderHandler: Top Level Window is null");
      }
    }
    _currentState = _guiState.getState();
    _guiState.setState( GUIBuilderState.BORDER_STATE );
  }
  
  /**
   * This method is invoked when the processing of the subtree 
   * is complete.
   */
  public void subTreeComplete() {
    _guiState.setState(_currentState);  
  }
}