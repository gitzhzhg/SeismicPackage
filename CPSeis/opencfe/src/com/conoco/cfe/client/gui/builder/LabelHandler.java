package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.LabelControl;
import com.conoco.cfe.client.gui.controls.WindowControl;

import javax.swing.JPanel;

import org.w3c.dom.CharacterData;
import org.w3c.dom.Node;

/**
 * A handler for the "Label" node.
 */
public class LabelHandler extends DontProcessSubTree {
    
  /**
   * Constructs a new handler object.
   * 
   * @param guiState   the state object which is manipulated
   *           by this handler
   */
  public LabelHandler(GUIBuilderState guiState) {
    super(guiState);
  }
   
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree.
   * 
   * @param node the node to be processed
   */
  public void performAction(Node node) {
    String keyword = XMLHelper.getStringAttributeValue(node, "keyword");
    String alignment = XMLHelper.getStringAttributeValue(node, "alignment");
    String label = XMLHelper.getCharDataString( node);

    LabelControl labelControl = new LabelControl(label);
    labelControl.setKeyword(keyword);
    labelControl.setAlignment(alignment);
    _guiState.registerControl(labelControl);
        
    if ((_guiState.getState() == GUIBuilderState.COMMAND_STATE) ||
        (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ||
        (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
      
      if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {   
        if ( _guiState.getScreen() != null ) {    
          _guiState.getScreen().add(labelControl);
        } 
        else {
          Console.logMessage("LabelHandler: Screen Control is null");  
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getBottomComponentPanel();
          p.add(labelControl.getComponent());
        } 
        else {
          Console.logMessage("LabelHandler: Top Level Window is null");
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getTopComponentPanel();
          p.add(labelControl.getComponent());
        } 
        else {
          Console.logMessage("LabelHandler: Top Level Window is null");
        }
      }      
    } 
    
    else if ( _guiState.getState() == GUIBuilderState.MENU_STATE) {
      if ( _guiState.getMenu() != null ) {
        _guiState.getMenu().add(labelControl);  
      } 
      else {
        Console.logMessage("LabelHandler: Menu Control is null");  
      }
    }
  } 
}