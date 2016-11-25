///
/// CommandAreaControl.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-18-2001 SMCook   Added setHelpPanel() function as part of solution
///                          to disappearing help tip problem.
///                         Added 'public' modifier to functions that were
///                          defaulting to public.
///

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Console;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Dimension;
import java.awt.Color;
import java.awt.LayoutManager;

import java.util.Enumeration;
import java.util.Vector;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.BorderFactory;

/**
 * A special control for the command area panel. The command
 * area is where the all the screens and the top and the bottom
 * button bars reside. The command area also has the responsibility
 * for controlling the position of help tip panel.
 */
public class CommandAreaControl  extends GUIControlAdapter {
  /**
   * Declares a variable for the main panel component
   * 
   * @serial
   */
  protected JPanel _cmdArea;  
  
  /**
   * Variable for the panel for holding the bottom buttons
   * 
   * @serial
   */
  protected JPanel _bottomButtonPanel;
  
  /**
   * Variable for the panel for holding the top buttons
   * 
   * @serial
   */
  protected JPanel _topButtonPanel;
  
  /**
   * Declares a variable for the top panel component
   * which contains the top toolbar buttons
   * 
   * @serial
   */
  protected JPanel _top;
  
  /**
   * Declares a variable for the bottom panel component
   * which contains the bottom toolbar buttons
   * 
   * @serial
   */
  protected JPanel _bottom;
  
  /**
   * Declares a variable for the bottom panel component
   * which contains the tab pane
   * 
   * @serial
   */
  protected JPanel _middle;
  
  /**
   * Declares a variable for the panel that displays the help
   * tip.
   * 
   * @serial
   */
  protected JPanel _helpPanel;
  
  /**
   * Declares a variable for the controls that are contained 
   * by this command area.
   * 
   * @serial
   */
  protected Vector _controls;
  
  /**
   * Boolean variable that is set if top area buttons are present;
   * false otherwise
   */
  protected boolean _isTopAreaPresent = false;
  
  /**
   * Boolean variable that is set if bottom area buttons are present;
   * false otherwise
   */
  protected boolean _isBottomAreaPresent = false;
  
  /**
   * Boolean variable that is set if help tip panel is present;
   * false otherwise
   */
  protected boolean _isHelpPresent = false;
  
  /**
   * Boolean variable that is set if top area buttons are added;
   * false otherwise
   */
  protected boolean _isTopAreaAdded = false;
  
  /**
   * Boolean variable that is set if bottom area buttons are added;
   * false otherwise
   */
  protected boolean _isBottomAreaAdded = false;

  /**
   * Boolean variable that is set if help tip panel is added;
   * false otherwise
   */
  protected boolean _isHelpAdded = false;
  
  /**
   * Variable for specifying the position of help panel.
   */
  protected String _helpPosition = "bottom";
  
  /**
   * Constructs a new command area control.
   */
  public CommandAreaControl()  {     
    super();
    _controls = new Vector();
    initTopPanel();
    initBottomPanel();  
    initMiddlePanel();
    initHelpPanel();
    initCommandAreaPanel();
  }

  /**
   * Protected method called by the constructor for top
   * panel creation.
   */
  protected void initTopPanel() {
    _top = new JPanel(new BorderLayout());
    _topButtonPanel = new MyPanel(new FlowLayout(FlowLayout.LEFT));
    _top.add(_topButtonPanel, BorderLayout.CENTER);
  }

  /**
   * Protected method called by the constructor for bottom
   * panel creation.
   */
  protected void initBottomPanel() {
    _bottom = new JPanel(new BorderLayout());
    _bottomButtonPanel = new MyPanel(new FlowLayout(FlowLayout.LEFT));
    _bottom.add(_bottomButtonPanel, BorderLayout.CENTER);
  }
  
  /**
   * Protected method called by the constructor for middle
   * panel creation.
   */
  protected void initMiddlePanel() {
    _middle = new JPanel();
    _middle.setLayout(new BorderLayout());
  }

  /**
   * Protected method called by the constructor for help
   * panel creation.
   */
  protected void initHelpPanel() {
    _helpPanel = new JPanel();
    _helpPanel.setBorder(BorderFactory.createLineBorder(Color.gray, 2));
    _helpPanel.setOpaque(true);
    JLabel l = new JLabel(" ");
    l.setFont ( ClientConstants.getDefaultLabelFont());
    _helpPanel.add(l);
  }
  
  /**
   * Protected method called by the constructor for command area
   * panel creation.
   */
  protected void initCommandAreaPanel() {
    _cmdArea = new JPanel(new BorderLayout());
    _cmdArea.add(_middle, BorderLayout.CENTER);          
  }

  /**
   * Returns the container that contains the top area component.
   * 
   * @return   the container containing the top area component; 
   *     <code>null</code> if top area component is not present
   */
  public JPanel getTopComponentPanel() {
    if ( _isTopAreaPresent ) {
      return _topButtonPanel;  
    } 
    else {  
      return null;
    }
  }

  /**
   * Returns the container that contains the bottom area component.
   * 
   * @return   the container containing the bottom area component; 
   *     <code>null</code> if bottom area component is not present
   */
  public JPanel getBottomComponentPanel() {
    if ( _isBottomAreaPresent ) {
      return _bottomButtonPanel;  
    } 
    else {  
      return null;
    }  
  }
  
  /**
   * Re-layouts the various components of this command area. 
   * This is called when top or bottom area are activated 
   * or when the help panel position is changed.
   */
  protected void relayout() {
    if ( _isTopAreaPresent ) {
      addTopArea();  
    } 
    if ( _isBottomAreaPresent ) {
      addBottomArea();
    } 
    if ( _isHelpPresent ) {
      addHelpPanel();
    }
    _topButtonPanel.revalidate();
    _bottomButtonPanel.revalidate();
    _bottom.revalidate();
    _top.revalidate();
    _middle.revalidate();
    _cmdArea.revalidate();
  }
  
  /**
   * Adds the top area button panel to the command area.
   */
  protected void addTopArea() {
    if ( !_isTopAreaAdded ) {
      _isTopAreaAdded = true;
      _cmdArea.add(_top, BorderLayout.NORTH);    
    }
  }  
  
  /**
   * Adds the bottom area button panel to the command area.
   */
  protected void addBottomArea() {
    if ( !_isBottomAreaAdded ) {
      _isBottomAreaAdded = true;
      _cmdArea.add(_bottom, BorderLayout.SOUTH);
    }
  }  
  
  /**
   * Adds the help panel to the command area.
   */
  protected void addHelpPanel() {
    if ( !_isHelpAdded ) {
      _isHelpAdded = true;
      if ( _helpPosition.equals("topMost") ) {
        addTopArea();
        _top.add(_helpPanel, BorderLayout.NORTH);
      } 
      else if ( _helpPosition.equals("top") ) {
        addTopArea();
        _top.add(_helpPanel, BorderLayout.SOUTH);
      } 
      else if ( _helpPosition.equals("bottomMost") ) {
        addBottomArea();
        _bottom.add(_helpPanel, BorderLayout.SOUTH);
      } 
      else if ( _helpPosition.equals("bottom") ) {
        addBottomArea();
        _bottom.add(_helpPanel, BorderLayout.NORTH);
      }
    }
  }  
  
  /**
   * Returns the GUI component contained by this control.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    relayout();      
    return _cmdArea;
  }

  /**
   * Adds a specified control to this control.
   * 
   * @param control the GUI control to be added to this control
   * @param constraint the constraint to be used while adding the control
   */
  public void add(GUIControl control, Object constraint) {
    _controls.addElement(control);
    _middle.add(control.getComponent(), constraint);
  }
  
  /**
   * Sets the JPanel that contains the help tip.  //SMCook
   */
  public void setHelpPanel(JPanel p) {
    _helpPanel = p;
  }  
  
  /**
   * Returns the panel component that contains the help tip.
   * 
   * @return the panel component that is contains the help tip
   */
  public JPanel getHelpPanel() {
    return _helpPanel;
  }  
  
  /**
   * Adds the top button panel to the command area.
   */
  public void activateTopArea() {
    _isTopAreaPresent = true;
    relayout();
    _cmdArea.repaint();  
  }
  
  /**
   * Adds the bottom button panel to the command area.
   */
  public void activateBottomArea() {
    _isBottomAreaPresent = true;
    relayout();
    _cmdArea.repaint();  
  }
  
  /**
   * Sets the help panel position within this command area.
   */
  public void setHelpPanelPosition(String position) {
    _isHelpPresent = true;
    _helpPosition = position;
    relayout();
    _cmdArea.repaint();  
  }
  
  /**
   * Disposes this control.
   */
  public void dispose() {
    _helpPanel.removeAll();
    _top.removeAll();
    _middle.removeAll();
    _bottom.removeAll();
    _cmdArea.remove(_middle);
    Enumeration en = _controls.elements();
    while ( en.hasMoreElements()) {
      ((GUIControl) en.nextElement()).dispose();
    }
    _controls.removeAllElements();
    super.dispose();
  }                

  private class MyPanel extends JPanel {
    /**
     * Constructs a new screen object.
     */
    public MyPanel() {
      super();
    }

    /**
      */
    public MyPanel(LayoutManager lm) {
      super(lm);
    }
    
    /**
     * Adds a specified component with the specified constraints.
     * 
     * @param comp the component to be added
     * @param constraints   the constraints object using which the 
     *             component is to be added
     * @param index      the position at which the component
     *             is to be added
     */
    protected void addImpl(Component comp, Object constraints, int index) {
       if ( comp instanceof JPanel ) {
         super.addImpl(comp, constraints, getComponents().length-1);
       } 
       else {
         super.addImpl(comp, constraints, index);
       }                     
     }
  }
}
