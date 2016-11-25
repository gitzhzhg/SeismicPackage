// HelpPanelHandler.java

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.GUIControlAdapter;
import com.conoco.cfe.client.gui.controls.BorderPanelControl;

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
 * A handler for the "HelpPanel" node in the document that describes
 * the GUI layout.
 */
public class HelpPanelHandler extends ProcessSubTree {
  
  /**
   * Constructs a new handler object.
   * 
   * @param guiState   the state object which is manipulated
   *           by this handler
   */
  public HelpPanelHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the current part of the tree.
   * 
   * @param node the DOM tree node which is to be processed
   */
  public void performAction(Node node) {
    Hashtable attrs = XMLHelper.getAttributesAsHashtable(node);
    Object keyword = attrs.get("keyword");
    Object position = attrs.get("position");
        
    if ( _guiState.getTopLevelWindow() != null ) {
      _guiState.getTopLevelWindow().setHelpPanelPosition((String) position);
    } 
    else {
      Console.logMessage("HelpPanelHandler: Window control is null");  
    }
  }
}