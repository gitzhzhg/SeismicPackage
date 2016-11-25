// GUIBuilder.java

package com.conoco.cfe.client.gui;

import com.conoco.cfe.client.gui.builder.HelpSection;

import com.conoco.cfe.client.gui.controls.WindowControl;
import com.conoco.cfe.client.gui.controls.MenuBarControl;
import com.conoco.cfe.client.gui.controls.ToolBarControl;
import com.conoco.cfe.client.gui.controls.TabPaneControl;

import java.awt.Window;

import java.util.Hashtable;
import java.util.Vector;

/**
 * Interface that describes the general behaviour of a 
 * GUI builder. The builder instantiates and lays out the 
 * components in a container. 
 */
public interface GUIBuilder {
    
  /**
   * Returns the action handlers used by this builder
   * to convert DOM Tree elements into GUI components
   * 
   * @return the hashtable of keyword to ActionHandler
   */
  public Hashtable getActionHandlers();
   
  /**
   * Returns the list of created GUIControls
   *
   * @return the hashtable of created controls, keyed by keyword
   */
  public Hashtable getGUIControls();
  
  /**
   * Returns the Window containing all other controls
   *
   * @return the Window component
   */
  public WindowControl getTopLevelWindow();
  
  /**
   * Returns the menu bar control contained by the window.
   * 
   * @return the menu bar control
   */
  public MenuBarControl getMenuBar();
  
  /**
   * Returns the tool bar control contained by the window.
   * 
   * @return the window control
   */
  public ToolBarControl getToolBar();
  
  /**
   * Returns the tab pane contained by the window.
   * 
   * @return the tab pane control
   */
  public TabPaneControl getTabPane();
  
  /**
   * Returns the help section object. The help section 
   * encapsulates the help tip and help text information
   * for all the components in this window.
   * 
   * @return   the help section object associated with 
   *       the window created by this builder
   */
  public HelpSection getHelpSection();
}