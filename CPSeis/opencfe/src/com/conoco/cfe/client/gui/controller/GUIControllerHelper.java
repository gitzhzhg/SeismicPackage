// GUIControllerHelper.java

package com.conoco.cfe.client.gui.controller;

import com.conoco.cfe.client.gui.builder.HelpSection;

import com.conoco.cfe.client.gui.controls.GUIControl;
import com.conoco.cfe.client.gui.controls.WindowControl;
import com.conoco.cfe.client.gui.controls.TabPaneControl;

import java.util.Hashtable;

/**
 * A helper that helps the GUI controller.
 */
public class GUIControllerHelper {
  /**
   * Lookup table that maps a window id with
   * the GUI controls constructed by the 
   * GUI builder for that particular window
   * 
   * @serial
   */
  protected static Hashtable _windowIdControlsLookup = new Hashtable();
  
  /**
   * Lookup table that maps a window id with
   * the tab pane for that window
   *
   * @serial
   */
  protected static Hashtable _windowIdTabPaneLookup = new Hashtable();
  
  /**
   * Lookup table that maps a window id with 
   * a window
   * 
   * @serial
   */
  protected static Hashtable _windowIdWindowLookup = new Hashtable();
  
  /**
   * Lookup table that maps a window id with 
   * the window help section
   * 
   * @serial
   */
  protected static Hashtable _windowIdHelpSectionLookup = new Hashtable();
  
  /**
   * A lookup table for the reply action handlers
   *
   * @serial
   */
  protected static Hashtable _replyActionHandlers;
    
  /**
   * Maps the GUI controls contained in a window 
   * with the window id.
   * 
   * @param winId the window id
   * @param controls the controls in the window 
   */
  static void putWindowIdControlsData(int winId, Hashtable controls) {
    _windowIdControlsLookup.put(new Integer(winId), controls);
  }
  
  /**
   * Maps a window control with its id.
   * 
   * @param winId the window id
   * @param wc the window control
   */
  static void putWindowIdWindowData(int winId, WindowControl wc) {
    _windowIdWindowLookup.put(new Integer(winId), wc);
  }
  
  /**
   * Maps a tab pane control ( in a window ) with a window
   * id.
   * 
   * @param winId the window id
   * @param wc the tab pane control
   */
  static void putWindowIdTabPaneData(int winId, TabPaneControl tbc) {
    _windowIdTabPaneLookup.put( new Integer(winId), tbc);
  }
  
  /**
   * Maps a help section with a window id.
   * 
   * @param winId the window id
   * @param hs the help section
   */
  static void putWindowIdHelpSectionData(int winId, HelpSection hs) {
    _windowIdHelpSectionLookup.put( new Integer(winId), hs);
  }
  
  /**
   * Returns the lookup table that maps a window id with the
   * controls in the window.
   * 
   * @return the window id versus GUI controls lookup
   */
  public static Hashtable getWindowIdToControls() {
    return _windowIdControlsLookup;
  }
  
  /**
   * Returns the lookup table that maps a window id with the
   * window control
   * 
   * @return the window id versus window control lookup
   */
  public static Hashtable getWindowIdToWindow() {
    return _windowIdWindowLookup;
  }
  
  /**
   * Returns the lookup table that maps a window id with the
   * controls in the window.
   * 
   * @return the window id versus GUI controls lookup
   */
  public static Hashtable getWindowIdToHelpSection() {
    return _windowIdHelpSectionLookup;
  }
  
  /**
   * Returns the lookup table that maps a window id with the
   * tab pane control in the window.
   * 
   * @return the window id versus the tab pane control lookup
   */
  public static Hashtable getWindowIdToTabPane() {
    return _windowIdTabPaneLookup;
  }    
  
  /**
   * Removes an entry from window id-controls lookup table.
   * 
   * @param pid the id of the window whose entry is to be removed  
   */
  static void removeWindowIdControlsData(int pid) {
    _windowIdControlsLookup.remove( new Integer(pid));  
  }
  
  /**
   * Removes an entry from window id-window lookup table.
   * 
   * @param pid the id of the window whose entry is to be removed  
   */
  static void removeWindowIdWindowData(int pid) {
    _windowIdWindowLookup.remove( new Integer(pid));  
  }
  
  /**
   * Removes an entry from window id-tab pane lookup table.
   * 
   * @param pid the id of the window whose entry is to be removed  
   */
  static void removeWindowIdTabPaneData(int pid) {
    _windowIdTabPaneLookup.remove(new Integer(pid));  
  }
  
  /**
   * Removes an entry from window id-help section lookup table.
   * 
   * @param pid the id of the window whose entry is to be removed  
   */
  static void removeWindowIdHelpSectionData( int pid) {
    _windowIdHelpSectionLookup.remove( new Integer(pid));
  }
}