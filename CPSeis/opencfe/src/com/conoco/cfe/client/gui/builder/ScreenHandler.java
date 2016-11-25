// ScreenHandler.java

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.ScreenControl;
import com.conoco.cfe.client.gui.controls.WindowControl;
import com.conoco.cfe.client.gui.controls.CellLayout;

import javax.swing.JPanel;

import org.w3c.dom.Node;

/**
 * A handler for the "Screen" node.
 */
public class ScreenHandler extends ProcessSubTree {

  /**
   * Constructs a new arrayset handler.
   * 
   * @param guiState   the state object that will be acted upon by this
   *           handler
   */  
  public ScreenHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree
   * 
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
    String keyword = XMLHelper.getStringAttributeValue(node, "keyword");
    String title   = XMLHelper.getStringAttributeValue(node, "title");
    int    columns = XMLHelper.getIntAttributeValue(node, "columns");
    int    rows    = XMLHelper.getIntAttributeValue(node, "rows");
    int    minCellWidth = XMLHelper.getIntAttributeValue(node, "minCellWidth");
    int    minCellHeight    = XMLHelper.getIntAttributeValue(node, "minCellHeight");

    if ( _guiState.getState() == GUIBuilderState.COMMAND_STATE ) {
      ScreenControl screen = new ScreenControl(columns, rows, minCellWidth, minCellHeight);
      if ( GUIBuilderState.SCROLLABLE_SCREEN ) {    
        screen.setScrollable(true);        
      }
      screen.setKeyword(keyword);
      screen.setTitle(title);
      
      _guiState.registerControl(screen);
      _guiState.setScreen(screen);

      if ( _guiState.getTabPane() != null ) {
        _guiState.getTabPane().add(screen);
      } 
      else {
        Console.logMessage("ScreenHandler: Tab Pane control is null");
      }
    } 
    
    else if ( _guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE ) {
      WindowControl wc = _guiState.getTopLevelWindow();
      if ( wc != null ) {
        JPanel top = wc.getTopComponentPanel();
        top.setLayout(new CellLayout(columns, rows, minCellWidth, minCellHeight));
      } 
      else {
        Console.logMessage("ScreenHandler: Top Level Window control is null");
      }
    } 
    
    else if ( _guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE ) {
      WindowControl wc = _guiState.getTopLevelWindow();
      if ( wc != null ) {
        JPanel bot = wc.getBottomComponentPanel();
        bot.setLayout(new CellLayout(columns, rows, minCellWidth, minCellHeight));
      } 
      else {
        Console.logMessage("ScreenHandler: Top Level Window control is null");
      }
    }
  }
  
  /**
   * This method is invoked when the processing of the subtree
   * is complete.
   */
  public void subTreeComplete() {
    if ( _guiState.getPopupMenu() != null ) {
      if ( _guiState.getScreen() != null ) {
        _guiState.getScreen().activatePopupMenu(
          _guiState.getPopupMenu());
      } 
      else {
        Console.logMessage("ScreenHandler: Screen control is null");
      }
      _guiState.setPopupMenu(null);
    }
  }
}