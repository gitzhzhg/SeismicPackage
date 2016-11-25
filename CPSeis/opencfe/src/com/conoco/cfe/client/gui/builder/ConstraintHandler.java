package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.XMLHelper;
import com.conoco.cfe.client.gui.controls.CellConstraints;
import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.CellLayout;
import com.conoco.cfe.client.gui.controls.WindowControl;

import javax.swing.JPanel;

import org.w3c.dom.Node;

/**
 * A handler for the "Constraint" node.
 */
public class ConstraintHandler extends ProcessSubTree {
  
  /** 
   * Constructs a new handler.
   * 
   * @param guiState   the state object on which this handler
   *           will act
   */
  public ConstraintHandler(GUIBuilderState guiState) {
    super(guiState);
  }
  
  /**
   * Pass the ActionHandler the DOM Tree node for 
   * the current part of the tree
   */
  public void performAction(Node node) {
    String component = XMLHelper.getStringAttributeValue(node, "component");      
    int xpos  = XMLHelper.getIntAttributeValue(node, "xPos");
    int ypos  = XMLHelper.getIntAttributeValue(node, "yPos");
    int xsize = XMLHelper.getIntAttributeValue(node, "xSize");
    int ysize = XMLHelper.getIntAttributeValue(node, "ySize");
    boolean xstretch = XMLHelper.getBooleanAttributeValue(node, "xStretch");
    boolean ystretch = XMLHelper.getBooleanAttributeValue(node, "yStretch");
    
    CellConstraints constraint = new CellConstraints();
    constraint.xPos     = xpos;
    constraint.yPos     = ypos;
    constraint.xSize    = xsize;
    constraint.ySize    = ysize;
    constraint.xStretch = xstretch;
    constraint.yStretch = ystretch;
    
    GUIControl control = _guiState.getControl(component.toUpperCase());
    
    if (control == null) {
      //Console.logMessage("ConstraintHandler: Couldn't find component " + component);
      return;
    }
    
    if ( (_guiState.getState() == GUIBuilderState.COMMAND_STATE) ) {
      _guiState.getScreen().setConstraint(control, constraint);
    }  
    
    else if ( (_guiState.getState() == GUIBuilderState.BOTTOM_AREA_COMPONENT_STATE) ) {
      WindowControl wc = _guiState.getTopLevelWindow();
      if ( wc != null ) {
        JPanel p = wc.getBottomComponentPanel();
        ((CellLayout) p.getLayout()).addLayoutComponent(control.getComponent(), constraint);
      } 
      else {
        Console.logMessage("ConstraintHandler: Top Level Window is null");
      }
    } 
    
    else if ( (_guiState.getState() == GUIBuilderState.TOP_AREA_COMPONENT_STATE) ) {
      WindowControl wc = _guiState.getTopLevelWindow();
      if ( wc != null ) {
        JPanel p = wc.getTopComponentPanel();
        ((CellLayout) p.getLayout()).addLayoutComponent(control.getComponent(), constraint);
      } 
      else {
        Console.logMessage("ConstraintHandler: Top Level Window is null");
      }
    }
  }
}
