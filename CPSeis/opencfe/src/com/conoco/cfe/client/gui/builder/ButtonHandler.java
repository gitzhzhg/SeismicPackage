package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.ButtonControl;
import com.conoco.cfe.client.gui.controls.MenuItemControl;
import com.conoco.cfe.client.gui.controls.ToggleButtonControl;
import com.conoco.cfe.client.gui.controls.WindowControl;

import javax.swing.JPanel;
import javax.swing.JToggleButton;

import org.w3c.dom.Node;

/**
 * A handler for the "Button" node.
 */
public class ButtonHandler extends DontProcessSubTree {
  
  /**
   * Creates a new handler.
   * 
   * @param guiState   the state object that is manipulated 
   *           ny this handler
   */
  public ButtonHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the current part of the tree
   * 
   * @param node the node to be processed
   */
  public void performAction(Node node) {
    String keyword = XMLHelper.getStringAttributeValue(node, "keyword");
    String label = XMLHelper.getCharDataString(node);    

    if ( label == null ) {
      label = keyword;  
    }
    
    if (_guiState.getState() == GUIBuilderState.MENU_STATE) {
      MenuItemControl menuItem = new MenuItemControl(label);
      menuItem.setKeyword(keyword);
      
      _guiState.registerControl(menuItem);
      if ( _guiState.getMenu() != null ) {
        _guiState.getMenu().add(menuItem);
      } 
      else {
        Console.logMessage("ButtonHandler: Menu Control is null");
      }  
    } 
    
    else if (_guiState.getState() == GUIBuilderState.TOOLBAR_STATE) {
      ButtonControl button = new ButtonControl(label);
      button.setKeyword(keyword);
      
      _guiState.registerControl(button);
      
      if( _guiState.getToolBar() != null ) { 
        _guiState.getToolBar().add(button);
      } 
      else {
        Console.logMessage("ButtonHandler: Toolbar Control is null");
      }  
    }
    
    else if ( _guiState.getState() == GUIBuilderState.POPUPMENU_STATE) {
      MenuItemControl menuItem = new MenuItemControl(label);
      menuItem.setKeyword(keyword);
      
      _guiState.registerControl(menuItem);
      _guiState.getPopupMenu().add(menuItem);
    } 
    
    else if ( _guiState.getState() == GUIBuilderState.RADIOBUTTON_STATE) {
      ToggleButtonControl tbc = new ToggleButtonControl(label);
      tbc.setKeyword(keyword);
      
      _guiState.registerControl(tbc);
      
      if ( _guiState.getRadioButtonGroup() != null ) {
        _guiState.getRadioButtonGroup().add((JToggleButton) tbc.getComponent());
      }
      else {
        Console.logMessage("ButtonHandler: Radio Button Group is null");
      }  
      
      if ( _guiState.getScreen() != null ) {
        _guiState.getScreen().add(tbc);
      } 
      else {
        Console.logMessage("ButtonHandler: Screen Control is null");
      }  
    } 
    
    else {
      ButtonControl bc = new ButtonControl(label);  
      bc.setKeyword(keyword);
      _guiState.registerControl(bc);
      
      if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {
        if ( _guiState.getScreen() != null ) {
          _guiState.getScreen().add(bc);
        } 
        else {
          Console.logMessage("ButtonHandler: Screen Control is null");
        }  
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getBottomComponentPanel();
          p.add(bc.getComponent());
        } 
        else {
          Console.logMessage("ButtonHandler: Top Level Window is null");
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getTopComponentPanel();
          p.add(bc.getComponent());
        } 
        else {
          Console.logMessage("ButtonHandler: Top Level Window is null");
        }
      }
    }
  }
}