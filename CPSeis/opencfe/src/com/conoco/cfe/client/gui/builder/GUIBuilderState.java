///
/// GUIBuilderState.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 08-23-2002 SMCook   Added utility function listControls().
///

package com.conoco.cfe.client.gui.builder;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.ArraySetControl;
import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.MenuBarControl;
import com.conoco.cfe.client.gui.controls.MenuControl;
import com.conoco.cfe.client.gui.controls.ToolBarControl;
import com.conoco.cfe.client.gui.controls.TabPaneControl;
import com.conoco.cfe.client.gui.controls.ScreenControl;  
import com.conoco.cfe.client.gui.controls.WindowControl;  
import com.conoco.cfe.client.gui.controls.PopupMenuControl;  

import com.conoco.cfe.utils.ArrayList;

import java.util.Enumeration;
import java.util.Hashtable;

import javax.swing.ButtonGroup;
import javax.swing.JPanel;

/**
 * Implements a state machine for the GUI builder. 
 * The state machine is driven by a set of action handlers
 * which handle nodes in the document that describes the 
 * GUI layout.
 */  
public class GUIBuilderState {
  
  /**
   * A constant to identify that the state machine
   * is in the "Menu" state.
   * 
   * @serial
   */
  public final static int MENU_STATE    = 1;
  
  /**
   * A constant to identify that the state machine
   * is in the "ToolBar" state.
   * 
   * @serial
   */
  public final static int TOOLBAR_STATE = 2;
  
  /**
   * A constant to identify that the state machine
   * is in the "Command" state.
   * 
   * @serial
   */
  public final static int COMMAND_STATE = 3;
  
  /**
   * A constant to identify that the state machine
   * is in the "ArraySet" state.
   * 
   * @serial
   */
  public final static int ARRAYSET_STATE = 4;
  
  /**
   * A constant to identify that the state machine
   * is in the "Popup" state.
   * 
   * @serial
   */
  public final static int POPUPMENU_STATE = 5;
  
  /**
   * A constant to identify that the state machine
   * is in the "RadioButton" state.
   * 
   * @serial
   */
  public final static int RADIOBUTTON_STATE = 6;
  
  /**
   * A constant to identify that the state machine
   * is in the "Border" state.
   * 
   * @serial
   */
  public final static int BORDER_STATE = 7;  

  /**
    * A constant to identify that the state machine is in
   * the "TopAreaComponent" state.
   * 
   * @serial
       */
  public final static int TOP_AREA_COMPONENT_STATE = 8;

  /**
    * A constant to identify that the state machine is in
   * the "BottomAreaComponent" state.
   * 
   * @serial
       */
  public static final int BOTTOM_AREA_COMPONENT_STATE = 9;

  /**
    * A boolean variable that is set to true if a given 
    * screen is scrollable.
   * 
   * @serial
   */
  public static boolean SCROLLABLE_SCREEN = false;
  
  /**
   * Variable for the top window control.
   * 
   * @serial
   */
  private WindowControl   _windowControl;
  
  /**
   * Variable for the menubar control.
   * 
   * @serial
   */
  private MenuBarControl  _menuBar;
  
  /**
   * Variable for the menu control.
   * 
   * @serial
   */
  private MenuControl    _menuControl;
  
  /**
   * Variable for the toolbar control.
   * 
   * @serial
   */
  private ToolBarControl  _toolBar;
  
  /**
   * Variable for the tab pane control.
   * 
   * @serial
   */
  private TabPaneControl  _tabPane;
  
  /**
   * Variable for the screen control.
   * 
   * @serial
   */
  private ScreenControl  _screen;
  
  /**
   * Variable for the arrayset control.
   * 
   * @serial
   */
  private ArraySetControl _arraySet;
  
  /**
   * Variable for the popup menu control.
   * 
   * @serial
   */
  private PopupMenuControl _popUp;
  
  /**
   * Variable to store the current state
   * 
   * @serial
   */
  private int        _state;
  
  /**
   * Variable for the radio button group
   * 
   * @serial
   */
  private ButtonGroup   _radioButtonGroup;
  
  /**
   * Variable for storing the GUI controls that 
   * are created by the builder.
   * 
   * @serial
   */
  private Hashtable    _guiControls;
  
  /**
   * Variable for holding the key of the component 
   * whose help is being processed
   * 
   * @serial
   */
  private String       _currentHelpComponent;
  
  /**
   * Variable for the help section
   * 
   * @serial
   */
  private HelpSection    _helpSection;
  
  /**
   * Constructs a new state object.
   */  
  public GUIBuilderState() {
    _guiControls = new Hashtable();
    _helpSection = new HelpSection();
  }

  /**
   * Returns the top level window constructed by the 
   * builder
   * 
   * @return the window control
   */
  public WindowControl getTopLevelWindow() {
    return _windowControl;
  }
  
  /**
   * Sets the top level window control.
   * 
   * @param w the top level window control to be set on this state
   */
  public void setTopLevelWindow(WindowControl w) {
    _windowControl = w;
  }
  
  /**
   * Returns the menubar control.
   * 
   * @return the menubar control
   */
  public MenuBarControl getMenuBar() {
    return _menuBar;
  }
  
  /**
   * Sets the menubar control on the GUI builder state.
   * 
   * @param m the menubar control to be set
   */
  public void setMenuBar(MenuBarControl m) {
    _menuBar = m;
  }

  /**
   * Returns the popup menu control.
   * 
   * @return the popup menu control.
   */
  public PopupMenuControl getPopupMenu() {
    return _popUp;
  }
  
  /**
   * Sets the popup menu control.
   * 
   * @param popUp the popup menu control 
   */
  public void setPopupMenu(PopupMenuControl popUp) {
    _popUp = popUp;
  }
  
  /**
   * Returns the menu control.
   * 
   * @return the menu control
   */
  public MenuControl getMenu() {
    return _menuControl;
  }
  
  /**
   * Sets the menu control.
   * 
   * @param m the menu control
   */
  public void setMenu(MenuControl m) {
    _menuControl = m;
  }
  
  /**
   * Returns the toolbar control.
   * 
   * @return the toolbar control
   */
  public ToolBarControl getToolBar() {
    return _toolBar;
  }
  
  /**
   * Sets the toolbar control.
   * 
   * @param t the toolbar control
   */
  public void setToolBar(ToolBarControl t) {
    _toolBar = t;
  }
  
  /**
   * Returns the tab pane control.
   * 
   * @return the tab pane control
   */
  public TabPaneControl getTabPane() {
    return _tabPane;
  }
  
  /**
   * Sets the tab pane control.
   * 
   * @param tab the tab pane control
   */
  public void setTabPane(TabPaneControl tab) {
    _tabPane = tab;
  }
  
  /**
   * Returns the screen control.
   * 
   * @return the screen control
   */
  public ScreenControl getScreen() {
    return _screen;
  }
  
  /**
   * Sets the screen control.
   * 
   * @param s the screen control to be set on this state
   */
  public void setScreen(ScreenControl s) {
    _screen = s;
  }
  
  /**
   * Returns the arrayset control.
   * 
   * @return the arrayset control set on this set
   */
  public ArraySetControl getArraySet() {
    return _arraySet;
  }
  
  /**
   * Sets the arrayset control on this state.
   * 
   * @param arraySet the arrayset object to be set on this state 
   */
  public void setArraySet(ArraySetControl arraySet) {
    _arraySet = arraySet;
  }
  
  /**
   * Sets the radio button group object.
   * 
   * @param bg the radio button group object to be set on this state
   */
  public void setRadioButtonGroup(ButtonGroup bg) {
    _radioButtonGroup = bg;
  }
  
  /**
   * Returns the radio button group.
   * 
   * @return the radio button group
   */
  public ButtonGroup getRadioButtonGroup() {
    return _radioButtonGroup;
  }
  
  /**
   * Returns the GUI controls costructed by the GUI builder.
   * The controls are accessed through a hashtable lookup 
   * that maps a control key with the control.
   * 
   * @return   the GUI controls constructed by the GUI builder
   *       as a <code>java.util.Hashtable</code>
   */
  public Hashtable getGUIControls() {
    return _guiControls;
  }

  /**
   * Returns the current state the state machine is in.
   * 
   * @return the current state of the state machine
   */
  public int getState() {
    return _state;
  }
  
  /**
   * Utility to list the currently registered controls.
   */
  public void listControls() {
    Enumeration e = _guiControls.elements();
    while(e.hasMoreElements()) {
      Console.logMessage(this, e.nextElement().toString());
    }
  }
  
  /**
   * Sets the current state.
   * 
   * @param state the state to be set on the state machine
   */
  public void setState(int state) {
    _state = state;
  }
  
  /**
   * Registers a GUI control. 
   * 
   * @param control the GUI control that is to be registered
   */
  public void registerControl(GUIControl control) {
    _guiControls.put(control.getKeyword().toUpperCase(), control);
  }
  
  /**
   * Removes a specified control from the list of controls.
   * 
   * @param control the GUI control to be removed
   */
  public void removeControl(GUIControl control) {
    _guiControls.remove(control);
  }
  
  /**
   * Looks up and returns a GUI control using the specified
   * key.
   * 
   * @param keyword the key of the control to be looked up
   * @return   the GUI control; <code>null</code> if 
   *       control is not found
   */
  public GUIControl getControl(String keyword) {
    return (GUIControl) _guiControls.get(keyword);
  }
  
  /**
   * Registers a component help tip.
   * 
   * @param keyword the keyword of the component
   * @param tip the help tip to be registered
   */
  public void addHelpTip(String keyword, String tip) {
    _helpSection.putHelpTip(keyword.toUpperCase(), tip);
  }

  /**
   * Registers a component help text.
   * 
   * @param keyword the keyword of the component
   * @param text the help text to be registered
   */  
  public void addHelpText(String keyword, String text) {
    _helpSection.putHelpText(keyword.toUpperCase(), text);
  }
  
  /**
   * Sets the current help component keyword.
   * 
   * @param key the keyword of the component
   */
  public void setCurrentHelpComponent(String key) {
    _currentHelpComponent = key;
  }
  
  /**
   * Returns the keyword of the component whose help is being
   * processed.
   * 
   * @return the component keyword
   */
  public String getCurrentHelpComponent() {
    return _currentHelpComponent;
  }    
  
  /**
   * Returns the help section for a window.
   * 
   * @return the help section for a window
   */
  public HelpSection getHelpSection() {
    return _helpSection;
  }    
  
  /**
   * Releases references to objects
   */
  public void dispose() {
    _windowControl = null;
    _menuBar = null;
    _menuControl = null;
    _toolBar = null;
    _tabPane = null;
    _screen = null;
    _arraySet = null;
    _popUp = null;
    _radioButtonGroup = null;
    _guiControls = null;
    _currentHelpComponent = null;
    _helpSection = null;
  }
}
