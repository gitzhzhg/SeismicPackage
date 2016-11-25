///
/// HelpWindowControl.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  9. 09-23-2003 SMCook   Change to HyperlinkJScrollPane to allow help
///                          system to have live links.
///  8. 05-20-2003 SMCook   ClassCastException that sometimes occurs on startup
///                          is caught, and its message made less ominous.
///  7. 10-25-2002 SMCook   Removed new static initializer and replaced it with
///                          a static "setup" function to serve basically the
///                          same purpose but with flexibility as to when it
///                          gets called.  Also, no longer needs xml URL info,
///                          which isn't available right at startup anyway.
///                          Class Application calls setup().
///  6. 10-24-2002 SMCook   Help window was failing under certain conditions.
///                          If TRIN or TROT was the first process called up,
///                          the help window would subsequently never work, and
///                          array index violation messages would repeatedly
///                          come from BoxView.java (Java source code).  Adding
///                          a new static initializer that forces early
///                          instantiation fixed the problem, and also allowed
///                          for the removal of lots of protective code that
///                          was checking whether the _control variable had
///                          been instantiated yet.
///                         Also cleaned up tabs, spaces, and alignment.
///  5. 08-13-2002 SMCook   Catches null _graphicsConfiguration cases now, and
///                          initializes this variable in only one place, so
///                          should be reliable now.
///  4. 08-02-2002 SMCook   Now pops up on second monitor, if available (except
///                          for specialty Exceed cases, in which case the
///                          help window behavior is as before).
///

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.controls.ui.MultiLineLabelToggleButtonUI;

import com.conoco.cfe.client.messaging.Preferences;
import com.conoco.cfe.client.messaging.URLReader;

import com.conoco.cfe.utils.EventQueue;

import java.awt.Font;
import java.awt.Component;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Rectangle;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentAdapter;

import javax.swing.JTextArea;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JScrollPane;
import javax.swing.JToggleButton;
import javax.swing.JEditorPane;

import javax.swing.text.DefaultCaret;

/**
 * A GUI control that encapsulates an Help Window. 
 */
public class HelpWindowControl  {
  /**
   */
  private static int COLUMNS = 75;

  /**
   */
  private static int WIDTH = 788;
 
  /**
   */
  private static int HEIGHT = 400;
 
  /**
   */
  protected static JTabbedPane _tabPane;
 
  /**
   */
  protected static JPanel _bottomPanel;

  /**
   */
   protected static JButton _remove;
 
  /**
   */
  protected static JFrame _frame;
 
  /**
   */
  protected static GraphicsConfiguration _graphicsConfiguration;

  /**
   */
  protected static JEditorPane _appHelp, _contextHelp;
  protected static HyperlinkJScrollPane _procHelp;

  /**
   */
  //protected static JScrollPane _appScroll, _procScroll, _contextScroll;
  protected static JScrollPane _appScroll, _contextScroll;
 
  /**
   */
  protected static boolean _isProcHelpAdded, _isAppHelpAdded;

  /**
   */
  private static HelpWindowControl _control;
 
  /**
   */
  protected static String _appHelpStr;

  /**
   */
  protected static Dimension _size = new Dimension(WIDTH, HEIGHT);
 
  /**
   */
  protected HelpWindowControl() {    
    try {
      GraphicsDevice[] screens =
        GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices();

      _graphicsConfiguration =
        screens[screens.length - 1].getDefaultConfiguration();
    }
    catch(Exception e) {
      _graphicsConfiguration = null;
    }

    createComponents();
    addComponents();
    try {
      setFont(new Font("Courier", Font.PLAIN, 12));
    }
    catch(ClassCastException e) {
      Console.logMessage(this, "Class Cast Exception");
    }
  }

  /**
   */
  public static void setup() {
    _control = new HelpWindowControl();
    _frame.pack();
  }

  /**
   */
  public static void show() {
    setContextHelp(null);
    setProcessHelp(_procHelp.getEditor().getText());
    setAppHelp(_appHelpStr);
    _frame.pack();
    _frame.setSize(_size);
    _frame.setVisible(true);
  }
 
  /**
   */
  protected void createComponents() {

    if(_graphicsConfiguration == null)
      _frame = new JFrame("Help");
    else
      _frame = new JFrame("Help", _graphicsConfiguration);

    _tabPane = new JTabbedPane();
    _bottomPanel = new JPanel();
    _remove = new JButton("Close");
    //_procHelp = new JEditorPane();
    _procHelp = new HyperlinkJScrollPane(_frame, null, 1000, 900);
    //_procHelp.setEditable(false);
    _procHelp.getEditor().setCaret(new AdjustableCaret(false));
    _appHelp = new JEditorPane();
    _appHelp.setEditable(false);
    _appHelp.setCaret(new AdjustableCaret(false));
    _contextHelp = new JEditorPane();
    _contextHelp.setEditable(false);
    _contextHelp.setCaret(new AdjustableCaret(false));

    _appScroll = new JScrollPane(_appHelp);
    _appScroll.setHorizontalScrollBarPolicy(
      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);      
    _appScroll.setVerticalScrollBarPolicy(
      JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);

    //_procScroll = new JScrollPane(_procHelp);
    //_procScroll.setHorizontalScrollBarPolicy(
    //  JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);      
    //_procScroll.setVerticalScrollBarPolicy(
    //  JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);

    _contextScroll = new JScrollPane(_contextHelp);
    _contextScroll.setHorizontalScrollBarPolicy(
      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);      
    _contextScroll.setVerticalScrollBarPolicy(
      JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
  } 
 
  /**
   */
  protected void addComponents() {
    //_frame = new JFrame("Help");  //SMCook- _frame is now instantiated in
                                    //         createComponents() using a
                                    //         GraphicsConfiguration argument.
    _frame.getContentPane().setLayout( new BorderLayout());
    _frame.getContentPane().add(_tabPane, BorderLayout.CENTER);
    _remove.addActionListener( new CloseListener());
    _bottomPanel.add(_remove);
    _tabPane.addTab("Parameter Help", _contextScroll);
    _frame.getContentPane().add(_bottomPanel, BorderLayout.SOUTH);
  }
 
  /**
   */
  public static void setContextHelp(String help) {
    if ( help != null ) {
      if ( help.startsWith("<html>") ) {
        _contextHelp.setContentType("text/html");  
      } 
      else {
        _contextHelp.setContentType("text/plain");  
      }
      _contextHelp.setText(help);
      //Console.logMessage(
      //  "HelpWindowControl: content type is " + _contextHelp.getContentType());
    }
    else {
      _contextHelp.setText("");  
    }
  }
 
  /**
   */
  public static void setAppHelpURL(String url) {
    String fileName = null;
    if (!Preferences.getXMLDocumentPath().startsWith("file")) {
      fileName = "http://" + 
        Preferences.getServerName() +
          Preferences.getXMLDocumentPath() + 
          url;
    }
    else {
      fileName = Preferences.getXMLDocumentPath() + url;
    }
    if ( fileName != null ) {
      _appHelpStr = URLReader.read(fileName);
      setAppHelp(_appHelpStr);    
    }
  }
 
  /**
   */
  public static void setProcessHelp(String help) {
    if (_procHelp == null) return;
    if ( help != null ) {
      //Console.logMessage("HelpWindowControl: help = " + help);
      if ( help.startsWith("<html>") ) {
        _procHelp.getEditor().setContentType("text/html");  
      } 
      else {
        _procHelp.getEditor().setContentType("text/plain");  
      }
      _procHelp.getEditor().setText(help);
      validateTabPane();
      if ( !_isProcHelpAdded ) {
        _isProcHelpAdded = true;
        //_tabPane.add("Process Overview Help", _procScroll);    
        _tabPane.add("Process Overview Help", _procHelp);    
        validateTabPane();
      }
    } 
    else {
      _procHelp.getEditor().setText("");  
    }
  }
 
  /**
   */
  public static void removeProcessHelp() {
    if ( _control != null ) {    
      _isProcHelpAdded = false;
      //_tabPane.remove(_procScroll);
      _tabPane.remove(_procHelp);
      validateTabPane();
    }
  }

  /**
   */
  protected static void setAppHelp(String help) {
    if ( help != null ) {
      if ( help.startsWith("<html>") ) {
        _appHelp.setContentType("text/html");  
      } 
      else {
        _appHelp.setContentType("text/plain");  
      }
      _appHelp.setText(help);
      if ( !_isAppHelpAdded ) {
        _isAppHelpAdded = true;

        _tabPane.add("Application Overview Help", _appScroll);    
        validateTabPane();
      }
    }
    else {
      _appHelp.setText("");  
    }
  }
 
  /**
   */
  protected static void removeAppHelp() {
    if ( _control != null ) {
      _isAppHelpAdded = false;
      _tabPane.remove(_appScroll);
      validateTabPane();
    }
  }

  /**
   */
  private static void validateTabPane() {
    _tabPane.invalidate();
    _tabPane.validate();
    _tabPane.repaint();
  }
 
  /**
   */
/*
  private static void validateSize() {
    _frame.setSize(_size);
  }
*/
 
  /**
   * Sets the font on this control.
   * 
   * @param f the new font as <code>java.awt.Font</code>
   */
  public static void setFont(Font f) {
    int mcharWidth = _appHelp.getFontMetrics(_appHelp.getFont()).charWidth('M');
    WIDTH = mcharWidth * COLUMNS;
    _frame.setFont(f);
    _tabPane.setFont(f);
    _appHelp.setFont(f);
    _contextHelp.setFont(f);
    _procHelp.setFont(f);
    _remove.setFont(f);
  }

  /**
   */
  private static class CloseListener implements ActionListener {
    /**
     */
    public void actionPerformed(ActionEvent e) {
      _size = _frame.getSize();    //retain current size
      _frame.setVisible(false);
    }  
  }

  /**
   */
  class AdjustableCaret extends DefaultCaret {
    private boolean adjust;
    public AdjustableCaret() {
      this(true);
    }
    public AdjustableCaret(boolean adjust) {
       super();
      this.adjust = adjust;
    }
    public void setAdjust(boolean adjust) {
      this.adjust = adjust;
    }
    public boolean getAdjust() {
      return adjust;
    }
    protected void adjustVisibility(Rectangle nloc) {
      if (adjust) {
         super.adjustVisibility(nloc);
      }
    }
  }
}
