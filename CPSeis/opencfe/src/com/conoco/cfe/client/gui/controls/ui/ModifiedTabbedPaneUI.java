// ModifiedTabbedPaneUI.java

package com.conoco.cfe.client.gui.controls.ui;

import java.awt.Graphics;

import javax.swing.JComponent;
import javax.swing.UIManager;

import javax.swing.plaf.ComponentUI;

import javax.swing.plaf.metal.MetalTabbedPaneUI;

/**
 * A UI delegate for a tabbed pane. The modified UI
 * does not paint the tab when the tabbed pane contains
 * only one tab.
 */
public class ModifiedTabbedPaneUI extends MetalTabbedPaneUI {
  /**
   * Variable for the static singleton UI delegate
   * 
   * @serial
   */  
  private final static ModifiedTabbedPaneUI ui = 
                            new ModifiedTabbedPaneUI();
  
  /**
   * Creates the UI delegate and returns the same.
   * 
   * @param the UI delegate for the specified component
   * @return the UI delegate
   */  
  public static ComponentUI createUI(JComponent c) {
    return ui;
  }
   
  /**
   * Installs this UI delegate as the default UI delegate
   * for tabbed panes.
   */
  public static void initialize() {
    String key = "TabbedPaneUI";
    Class cls = ui.getClass();
    String name = cls.getName();
    UIManager.put(key,name);
    UIManager.put(name,cls);
  }
    
  /**
   * Paints the specified component.
   * 
   * @param g the graphics context
   * @param c the component to be painted
   */  
  public void paint(Graphics g, JComponent c) {
    if ( tabPane.getTabCount() == 1 ) {
      int selectedIndex = tabPane.getSelectedIndex();
      int tabPlacement = tabPane.getTabPlacement();
      // Paint content border
      paintContentBorder(g, tabPlacement, selectedIndex);
    } 
    else {
      super.paint(g, c);
    }
  }
 
   /**
    * Paints the top border of the tab content. This 
    * method is required here because the superclass 
    * method paints the border taking into account the 
    * single tab as a result of which we get a broken border.
    * This method is overidden to take care of this problem.
    */
  protected void paintContentBorderTopEdge(Graphics g, int tabPlacement,
                                         int selectedIndex, 
                                         int x, int y, int w, int h) {
    if ( tabPane.getTabCount() == 1 ) {
      g.setColor(lightHighlight);
      g.drawLine(x, y, x+w, y);
    } 
    else {
      super.paintContentBorderTopEdge(g, tabPlacement, selectedIndex,
                                      x, y, w, h);
    }
  }
}