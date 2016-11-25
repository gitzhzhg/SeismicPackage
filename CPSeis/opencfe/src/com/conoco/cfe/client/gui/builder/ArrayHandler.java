///
/// ArrayHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-18-2001 SMCook   Changed constructor and import statements to use
///                          IndexedArrayControl, a new class combining the
///                          functionality of
///
///                           IndexedFloatArrayControl
///                           IndexedIntegerArrayControl
///                           IndexedStringArrayControl
///                           IndexedComboArrayControl (all 4 now eliminated).
///

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.ArrayGUIControlAdapter;
import com.conoco.cfe.client.gui.controls.ArraySetControl;
import com.conoco.cfe.client.gui.controls.ScreenControl;
import com.conoco.cfe.client.gui.controls.ViewRangeArrayControl;
import com.conoco.cfe.client.gui.controls.WindowControl;

import com.conoco.cfe.client.gui.controls.IndexedArrayControl;

import com.conoco.cfe.client.gui.controls.table.ColumnData;
import com.conoco.cfe.client.gui.controls.table.ArrayComponent;

import javax.swing.JPanel;

import org.w3c.dom.Node;

/**
 * A handler for the "Array" node in the document that 
 * describes a GUI layout. 
 */
public class ArrayHandler extends ProcessSubTree {
  
  /**
   * Constructs a new array handler object.
   * 
   * @param guiState   the state object which is manipulated
   *           by this handler
   */
  public ArrayHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the current part 
   * of the tree.
   * 
   * @param node the DOM tree node for the array component
   */
  public void performAction(Node node) {
    String keyword     = XMLHelper.getStringAttributeValue(node, "keyword").toUpperCase();
    String type       = XMLHelper.getStringAttributeValue(node, "type");
    int    maxLength   = XMLHelper.getIntAttributeValue(node, "maxLength");
    int    columnSize = XMLHelper.getIntAttributeValue(node, "columnSize");
    String columnName   = XMLHelper.getStringAttributeValue(node, "columnName");
    boolean editable   = XMLHelper.getBooleanAttributeValue(node, "editable");
    boolean sensitive    = XMLHelper.getBooleanAttributeValue(node, "sensitive");
    String clickMode    = XMLHelper.getStringAttributeValue(node, "clickMode");
    boolean sizeMaxLength;

    ArrayGUIControlAdapter array = new ArrayGUIControlAdapter(keyword);
    
    if (columnSize != 0){
      sizeMaxLength = false;
    }
    else {
      sizeMaxLength = true;
      columnSize = maxLength;
    }

    // if it a standalone array component
    if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ||
         (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ||
         (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE)  ) {

      if (type.equals("list")) {
        ViewRangeArrayControl c = new ViewRangeArrayControl(columnName);        
        c.setKeyword(keyword);

        c.setEditable(editable);
        c.setSensitive(sensitive);
        
        _guiState.registerControl(c);
    
        if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {
          ScreenControl screen = _guiState.getScreen();
          if ( screen != null ) {
            screen.add(c);
          } 
          else {
            Console.logMessage("ArrayHandler: Could not add array control: " + keyword);  
          }
        } 
        
        else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
          WindowControl wc = _guiState.getTopLevelWindow();
          if ( wc != null ) {
            JPanel p = wc.getBottomComponentPanel();
            p.add(c.getComponent());
          }
          else {
            Console.logMessage("ArrayHandler: Top Level Window is null");
          }
        } 
        
        else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
          WindowControl wc = _guiState.getTopLevelWindow();
          if ( wc != null ) {
            JPanel p = wc.getTopComponentPanel();
            p.add(c.getComponent());
          } 
          else {
            Console.logMessage("ArrayHandler: Top Level Window is null");
          }
        }
      } 

      else {
        if (type.equals("option")) {
        }
        else {
          array = new IndexedArrayControl(columnName,type);
        }

        ColumnData colData = ((ArrayComponent) array.getArrayComponent()).getColumnData(0);
        colData.setMaxLength(maxLength);
        colData.setColumnSize(columnSize, sizeMaxLength);

        array.setKeyword(keyword);
        array.setEditable(editable);
        array.setSensitive(sensitive);

        if ( clickMode.equalsIgnoreCase("ignore") ) 
        {
          array.setClickModeImmediate(false);        
        } 
        else {
          array.setClickModeImmediate(true);        
          if ( _guiState.getPopupMenu() != null ) {
            array.activatePopupMenu(_guiState.getPopupMenu());
          }
          else{
          }
        }
      
        _guiState.registerControl(array);
    
        if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) { 
          ScreenControl screen = _guiState.getScreen();
          if ( screen != null ) {
            screen.add(array);
          } 
          else {
            Console.logMessage("ArrayHandler: Could not add array control: " + keyword);  
          }
        } 

        else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
          WindowControl wc = _guiState.getTopLevelWindow();
          if ( wc != null ) {
            JPanel p = wc.getBottomComponentPanel();
            p.add(array.getComponent());
          } 
          else {
            Console.logMessage("ArrayHandler: Top Level Window is null");
          }
        } 
        
        else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
          WindowControl wc = _guiState.getTopLevelWindow();
          if ( wc != null ) {
            JPanel p = wc.getTopComponentPanel();
            p.add(array.getComponent());
          } 
          else {
            Console.logMessage("ArrayHandler: Top Level Window is null");
          }
        }
      }
    }

    // if it is an arrayset array component
    else if (_guiState.getState() == GUIBuilderState.ARRAYSET_STATE) {
      ArraySetControl arraySet = _guiState.getArraySet();
      if ( arraySet != null ) {
        Object col = arraySet.addColumn(keyword, type, maxLength, 
          columnSize, sizeMaxLength, columnName, editable, sensitive);
        _guiState.registerControl( (GUIControl) col);
      } 
      else {
        Console.logMessage("ArrayHandler: Could not add array control to arrayset: " + keyword);  
      }
    }
  }
}
