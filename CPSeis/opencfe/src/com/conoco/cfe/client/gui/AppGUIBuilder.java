// AppGUIBuilder.java

package com.conoco.cfe.client.gui;

import com.conoco.cfe.client.gui.builder.GUIBuilderState;
import com.conoco.cfe.client.gui.builder.HelpSection;
import com.conoco.cfe.client.gui.builder.GuiHandler;
import com.conoco.cfe.client.gui.builder.WindowHandler;
import com.conoco.cfe.client.gui.builder.MenuBarHandler;
import com.conoco.cfe.client.gui.builder.MenuHandler;
import com.conoco.cfe.client.gui.builder.ToolbarHandler;
import com.conoco.cfe.client.gui.builder.PopupMenuHandler;
import com.conoco.cfe.client.gui.builder.CommandAreaHandler;
import com.conoco.cfe.client.gui.builder.ScreenHandler;
import com.conoco.cfe.client.gui.builder.SeparatorHandler;
import com.conoco.cfe.client.gui.builder.LabelHandler;
import com.conoco.cfe.client.gui.builder.RadioButtonHandler;
import com.conoco.cfe.client.gui.builder.ButtonHandler;
import com.conoco.cfe.client.gui.builder.FieldHandler;
import com.conoco.cfe.client.gui.builder.ArrayHandler;
import com.conoco.cfe.client.gui.builder.ArraySetHandler;
import com.conoco.cfe.client.gui.builder.HelpHandler;
import com.conoco.cfe.client.gui.builder.LayoutHandler;
import com.conoco.cfe.client.gui.builder.ConstraintHandler;
import com.conoco.cfe.client.gui.builder.TipHandler;
import com.conoco.cfe.client.gui.builder.TextHandler;
import com.conoco.cfe.client.gui.builder.BorderHandler;
import com.conoco.cfe.client.gui.builder.HelpSectionHandler;
import com.conoco.cfe.client.gui.builder.HelpPanelHandler;
import com.conoco.cfe.client.gui.builder.DialogHandler;
import com.conoco.cfe.client.gui.builder.TopAreaComponentHandler;
import com.conoco.cfe.client.gui.builder.BottomAreaComponentHandler;
import com.conoco.cfe.client.gui.builder.ScrollableScreenHandler;

import com.conoco.cfe.client.gui.controls.ButtonControl;
import com.conoco.cfe.client.gui.controls.FloatFieldControl;
import com.conoco.cfe.client.gui.controls.IntFieldControl;
import com.conoco.cfe.client.gui.controls.MenuBarControl;
import com.conoco.cfe.client.gui.controls.MenuControl;
import com.conoco.cfe.client.gui.controls.MenuItemControl;
import com.conoco.cfe.client.gui.controls.ScreenControl;
import com.conoco.cfe.client.gui.controls.TabPaneControl;
import com.conoco.cfe.client.gui.controls.TextFieldControl;
import com.conoco.cfe.client.gui.controls.ToolBarControl;
import com.conoco.cfe.client.gui.controls.WindowControl;

import java.awt.BorderLayout;
import java.awt.Window;

import java.util.Hashtable;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import org.w3c.dom.Node;

/**
 * Implements the GUI builder that builds the GUI controls
 * from the document that describes the GUI layout. The 
 * builder delegates the processing to a set of action 
 * handlers. 
 * 
 * @see com.conoco.cfe.client.gui.builder
 */
public class AppGUIBuilder implements GUIBuilder {
  
  /**
   * Variable for the action handlers
   * 
   * @serial
   */
  private Hashtable     _actionHandlers;
  
  /**
   * Variable for the GUI builder state
   *
   * @serial
   */
  private GUIBuilderState _builderState;
  
  /**
   * Constructs a new builder object.
   */
  public AppGUIBuilder() {
    _actionHandlers = new Hashtable();
    _builderState   = new GUIBuilderState();
    createActionHandlers();
  }
  
  
  /**
   * Returns the action handlers used by this builder
   * to convert DOM Tree elements into GUI components
   * 
   * @return the hashtable of keyword to ActionHandler
   */
  public Hashtable getActionHandlers() {
    return _actionHandlers;
  }
   
  /**
   * Returns the list of indexed GUIControls by keyword
   *
   * @return the Hashtable of created controls
   */
  public Hashtable getGUIControls() {
    return _builderState.getGUIControls();
  }
  
  /**
   * Returns the Window containing all other controls
   *
   * @return the Window component
   */
  public WindowControl getTopLevelWindow() {
    return _builderState.getTopLevelWindow();
  }
  
  /**
   * Returns the menu bar control contained by the window.
   * 
   * @return the menu bar control
   */
  public MenuBarControl getMenuBar() {
    return _builderState.getMenuBar();  
  }
  
  /**
   * Returns the tool bar control contained by the window.
   * 
   * @return the window control
   */
  public ToolBarControl getToolBar() {
    return _builderState.getToolBar();  
  }
  
  /**
   * Returns the tab pane contained by the window.
   * 
   * @return the tab pane control
   */
  public TabPaneControl getTabPane() {
    return _builderState.getTabPane();  
  }

  /**
   * Returns the help section object. The help section 
   * encapsulates the help tip and help text information
   * for all the components in this window.
   * 
   * @return   the help section object associated with 
   *       the window created by this builder
   */
  public HelpSection getHelpSection() {
    return _builderState.getHelpSection();
  }

  /**
   * Creates the action handlers. This is a private method called
   * by the constructor.
   */
  private void createActionHandlers() {
    _actionHandlers.put("Gui",                 new GuiHandler(_builderState));
    _actionHandlers.put("Window",              new WindowHandler(_builderState));
    _actionHandlers.put("Dialog",              new DialogHandler(_builderState));
    _actionHandlers.put("MenuBar",             new MenuBarHandler(_builderState));
    _actionHandlers.put("Menu",                new MenuHandler(_builderState));
    _actionHandlers.put("Toolbar",             new ToolbarHandler(_builderState));
    _actionHandlers.put("PopupMenu",           new PopupMenuHandler(_builderState));
    _actionHandlers.put("CommandArea",         new CommandAreaHandler(_builderState));
    _actionHandlers.put("Screen",              new ScreenHandler(_builderState));
    _actionHandlers.put("Separator",           new SeparatorHandler(_builderState));
    _actionHandlers.put("Label",               new LabelHandler(_builderState));
    _actionHandlers.put("RadioButtons",        new RadioButtonHandler(_builderState));
    _actionHandlers.put("Button",              new ButtonHandler(_builderState));
    _actionHandlers.put("Field",               new FieldHandler(_builderState));
    _actionHandlers.put("Array",               new ArrayHandler(_builderState));
    _actionHandlers.put("ArraySet",            new ArraySetHandler(_builderState));
    _actionHandlers.put("Help",                new HelpHandler(_builderState));
    _actionHandlers.put("Layout",              new LayoutHandler(_builderState));
    _actionHandlers.put("Constraints",         new ConstraintHandler(_builderState));
    _actionHandlers.put("Tip",                 new TipHandler(_builderState));
    _actionHandlers.put("Text",                new TextHandler(_builderState));
    _actionHandlers.put("Border",              new BorderHandler(_builderState));
    _actionHandlers.put("HelpSection",         new HelpSectionHandler(_builderState));
    _actionHandlers.put("HelpPanel",           new HelpPanelHandler(_builderState));
    _actionHandlers.put("TopAreaComponent",    new TopAreaComponentHandler(_builderState));
    _actionHandlers.put("BottomAreaComponent", new BottomAreaComponentHandler(_builderState));    
    _actionHandlers.put("ScrollableScreen",    new ScrollableScreenHandler(_builderState));    
  }
  
  /**
   * Disposes this builder object by setting the object references
   * to null.
   */
  public void dispose() {
    _actionHandlers.clear();
    _actionHandlers = null;
    _builderState.dispose();
  }
}
