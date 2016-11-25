///
/// FieldHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  5.
///  4. 09-24-2001 SMCook   Added code to provide blank strings to the new
///                          ToggleButtonControl and ComboButtonControl widgets
///                          in order to try to improve their behavior when
///                          running under the XMLViewer.
///

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.IntFieldControl;
import com.conoco.cfe.client.gui.controls.FloatFieldControl;
import com.conoco.cfe.client.gui.controls.TextFieldControl;
import com.conoco.cfe.client.gui.controls.ComboBoxControl;
import com.conoco.cfe.client.gui.controls.ModifiedComboBoxControl;
import com.conoco.cfe.client.gui.controls.ToggleButtonControl;
import com.conoco.cfe.client.gui.controls.ComboButtonControl;
import com.conoco.cfe.client.gui.controls.WindowControl;

import javax.swing.JPanel;

import org.w3c.dom.Node;

/**
 * A handler for the "Field" node.
 */
public class FieldHandler extends ProcessSubTree {
  
  /**
   * Constructs a new field handler.
   * 
   * @param guiState   the state object on which this handler
   *           will act
   */
  public FieldHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the current part of the tree
   * 
   * @param node the node in the document to be processed
   */
  public void performAction(Node node) {
    String type    = XMLHelper.getStringAttributeValue(node, "type");
    String keyword = XMLHelper.getStringAttributeValue(node, "keyword");
    boolean isSensitive = XMLHelper.getBooleanAttributeValue(node, "sensitive");
    boolean isEditable = XMLHelper.getBooleanAttributeValue(node, "editable");
        
    if (type.equals("int")) {
      int maxLength = XMLHelper.getIntAttributeValue(node, "maxLength");
      IntFieldControl intField = new IntFieldControl();
      intField.setKeyword(keyword);
      intField.setColumns(maxLength);
      intField.setSensitive(isSensitive);
      intField.setEditable(isEditable);
         
      if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {     
        if ( _guiState.getScreen() != null ) {      
          _guiState.getScreen().add(intField);
        } 
        else {
          Console.logMessage("FieldHandler: Screen control is null");  
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getBottomComponentPanel();
          p.add(intField.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getTopComponentPanel();
          p.add(intField.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      }
      _guiState.registerControl(intField);
    }
    
    else if (type.equals("float")) {
      int maxLength = XMLHelper.getIntAttributeValue(node, "maxLength");
      FloatFieldControl floatField = new FloatFieldControl();
      floatField.setKeyword(keyword);
      floatField.setColumns(maxLength);
      floatField.setSensitive(isSensitive);
      floatField.setEditable(isEditable);
  
      if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {    
        if ( _guiState.getScreen() != null ) {
          _guiState.getScreen().add(floatField);
        } 
        else {
          Console.logMessage("FieldHandler: Screen control is null");  
        }
      } 

      else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getBottomComponentPanel();
          p.add(floatField.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getTopComponentPanel();
          p.add(floatField.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      }
      _guiState.registerControl(floatField);
    }
    
    else if (type.equals("string")) {
      int    maxLength = XMLHelper.getIntAttributeValue(node, "maxLength");
      TextFieldControl stringField = new TextFieldControl();
      stringField.setKeyword(keyword);
      stringField.setColumns(maxLength);
      stringField.setSensitive(isSensitive);
      stringField.setEditable(isEditable);
      
      if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {
        if ( _guiState.getScreen() != null ) {
          _guiState.getScreen().add(stringField);
        } 
        else {
          Console.logMessage("FieldHandler: Screen control is null");  
        }
      }  
      
      else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getBottomComponentPanel();
          p.add(stringField.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getTopComponentPanel();
          p.add(stringField.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      }
      _guiState.registerControl(stringField);
    }

    else if (type.equals("comboBox")) {      
      ComboBoxControl cbc = new ComboBoxControl();
      cbc.setKeyword(keyword);
      cbc.setSensitive(isSensitive);
      cbc.setEditable(isEditable);
    
      if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {   
        if ( _guiState.getScreen() != null ) {
          _guiState.getScreen().add(cbc);
        } 
        else {
          Console.logMessage("FieldHandler: Screen control is null");  
        } 
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getBottomComponentPanel();
          p.add(cbc.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getTopComponentPanel();
          p.add(cbc.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      }
      _guiState.registerControl(cbc);
    }

    else if ( type.equals("modifiedComboBox") ) {
      ModifiedComboBoxControl cbc = new ModifiedComboBoxControl();
      cbc.setKeyword(keyword);
      cbc.setSensitive(isSensitive);
      if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) { 
        if ( _guiState.getScreen() != null ) {
          _guiState.getScreen().add(cbc);
        } 
        else {
          Console.logMessage("FieldHandler: Screen control is null");  
        } 
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getBottomComponentPanel();
          p.add(cbc.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getTopComponentPanel();
          p.add(cbc.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      }
      _guiState.registerControl(cbc);
    }

    else if (type.equals("toggleButton")) {      
//SMCook - for XMLViewer, needs some dummy text to size properly
      int maxLength = XMLHelper.getIntAttributeValue(node, "maxLength");
      char[] ch=new char[maxLength];
      int i=0; while(i<maxLength) { ch[i]=' '; i++; }
      ToggleButtonControl tbc = new ToggleButtonControl(new String(ch));
      tbc.setKeyword(keyword);
      tbc.setSensitive(isSensitive);
      tbc.setEditable(isEditable);
    
      if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {   
        if ( _guiState.getScreen() != null ) {
          _guiState.getScreen().add(tbc);
        } 
        else {
          Console.logMessage("FieldHandler: Screen control is null");  
        } 
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getBottomComponentPanel();
          p.add(tbc.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getTopComponentPanel();
          p.add(tbc.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      }
      _guiState.registerControl(tbc);
    }

    else if (type.equals("comboButton")) {      
//SMCook - for XMLViewer, needs some dummy text to size properly
      int maxLength = XMLHelper.getIntAttributeValue(node, "maxLength");
      char[] ch=new char[maxLength];
      int i=0; while(i<maxLength) { ch[i]=' '; i++; }
      ComboButtonControl tbc = new ComboButtonControl(new String(ch));
      tbc.setKeyword(keyword);
      tbc.setSensitive(isSensitive);
      tbc.setEditable(isEditable);
    
      if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {   
        if ( _guiState.getScreen() != null ) {
          _guiState.getScreen().add(tbc);
        } 
        else {
          Console.logMessage("FieldHandler: Screen control is null");  
        } 
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getBottomComponentPanel();
          p.add(tbc.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      } 
      
      else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
        WindowControl wc = _guiState.getTopLevelWindow();
        if ( wc != null ) {
          JPanel p = wc.getTopComponentPanel();
          p.add(tbc.getComponent());
        } 
        else {
          Console.logMessage("FieldHandler: Top Level Window is null");
        }
      }
      _guiState.registerControl(tbc);
    }

    else {
      Console.logMessage("FieldHandler: Unknown field type.");
    }
  }
}
