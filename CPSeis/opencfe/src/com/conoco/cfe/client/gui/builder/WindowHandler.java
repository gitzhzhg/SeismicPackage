///
/// WindowHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  3. 09-29-2003 SMCook   Added winid to WindowControl constructor as part of
///                          scheme allowing parent/child tracking, which in
///                          turn produces good which-window-on-top behavior.
///

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.WindowControl;
import com.conoco.cfe.client.gui.XMLHelper;

import com.conoco.cfe.utils.EventQueue;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.JPanel;

import org.w3c.dom.Node;

/**
 * A handler for the "Window" node.
 */
public class WindowHandler extends ProcessSubTree {
  
  /**
   * Constructs a new handler object.
   * 
   * @param guiState   the state object which is manipulated
   *           by this handler
   */
  public WindowHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for the 
   * current part of the tree
   * 
   * @param node the node to be processed
   */
  public void performAction(Node node) {
    int width  = 0;
    int height = 0;
    int winid  = -1;
    try {
      width  = XMLHelper.getIntAttributeValue(node, "width"); 
      height = XMLHelper.getIntAttributeValue(node, "height"); 
      winid  = XMLHelper.getIntAttributeValue(node, "windowId"); 
    } 
    catch ( NullPointerException np) {
    }

    WindowControl windowControl = new WindowControl(winid);
    
    String keyword = XMLHelper.getStringAttributeValue(node, "keyword");
    
    if ( (width != 0) && (height != 0) ) {
      windowControl.setSize( new Dimension(width, height));
    }
    windowControl.setKeyword(keyword);
    _guiState.setTopLevelWindow(windowControl);
    _guiState.registerControl(windowControl); 
  }
}
