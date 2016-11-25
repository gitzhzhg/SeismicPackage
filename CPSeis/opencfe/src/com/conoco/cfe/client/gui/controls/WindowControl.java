///
/// WindowControl.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
/// 12. 10-17-2003 SMCook   Modified try-catch blocks for windowActivated and
///                          windowClosing functions -- Exception messages were
///                          showing up in /var/tmp logs.
/// 11. 10-08-2003 SMCook   Commented out Windows-platform workaround.
/// 10. 09-29-2003 SMCook   Added winid to constructor, and introduced _winVect
///                          and associated logic to maintain a list of window
///                          JFrame's.  This is part of scheme allowing for
///                          adequate parent/child tracking, which in turn
///                          allows for good which-window-is-on-top behavior.
///                         Also, modified to fix 1.4 Windows-platform-only bug
///                          wherein the main CFE gui would stubbornly pop back
///                          to the front when a new process screen was opened.
///                          Fix was to add a timer that prevents a window from
///                          reactivating "too quickly" after deactivating.
///                          Whether this is a bug in our code or in Java is
///                          still not clear.
///  9. 09-11-2002 SMCook   Modified to carry separate horizontal and vertical
///                          scaling factors.  Horizontal is currently set to
///                          1.00, vertical is set to 1.055 (the largest value
///                          that doesn't overrun an 1152x864 Exceed setup).
///  8. 08-23-2002 SMCook   Modal JDialogs now use the main CFE GUI JFrame from
///                          AppGUIController as their owner, improving which-
///                          window-belongs-on-top behavior.
///                         Cleaned up tabs.
///  7. 08-20-2002 SMCook   Added flicker suppression logic.
///                         Added hardwired scale factor sizing capability
///                          (currently set to 1.0, which is off).  This may be
///                          useful for some people (in which case a "prefs"
///                          variable would be needed), or as screens increase
///                          in resolution over the years.
///  6. 09-26-2001 SMCook   Handled dual-screen centering quirk (user request).
///  5. 09-24-2001 SMCook   Added window centering functionality.
///  4. 09-18-2001 SMCook   Added setHelpPanel() function as part of solution
///                          to disappearing help tip problem.
///

package com.conoco.cfe.client.gui.controls;

import com.conoco.cfe.client.ClientConstants;
import com.conoco.cfe.client.application.Console;
import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;
import com.conoco.cfe.client.gui.controller.AppGUIController;
import com.conoco.cfe.client.gui.messaging.ButtonPressActionHandler;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.Window;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JRootPane;
import javax.swing.JDialog;

/**
 * A GUI control that encapsulates a window component. The 
 * window is implemented either as a frame or a dialog. The
 * only difference between a frame component and a dialog 
 * component is that while a dialog-based window control 
 * can be set to be modal, a frame-based window control cannot.
 * 
 * @see com.conoco.cfe.client.gui.controls.CommandAreaControl
 */
public class WindowControl extends GUIControlAdapter {

  /**
   * Hardwired value allowing bulk scaling of window size
   * 
   * @serial
   */
  protected final double WINDOW_WSCALE_FACTOR = 1.00;
  protected final double WINDOW_HSCALE_FACTOR = 1.055;

  private long time1;

  /**
   * Variable for the frame component contained by this 
   * control
   * 
   * @serial
   */
  protected Window _frame;

  /**
   * Variable for the window id.
   * 
   * @serial
   */
  private int _id;

  /**
   * Color that matches overall GUI color.  Used to reduces harshness of
   * flicker during repaints.
   * 
   * @serial
   */
  protected final Color
    _flickerSuppressionColor = (new JPanel()).getBackground();

  /**
   * Variable for storing the window listener
   * 
   * @serial
   */
  protected WindowAdapter _windowListener;

  /** 
   * Variable for the menu bar control
   * 
   * @serial
   */
  protected MenuBarControl _menuBarControl;
 
  /**
   * Variable for the panel that contains the 
   * toolbar control
   * 
   * @serial
   */
  protected JPanel _topPanel;
 
  /**
   * Variable for the command area control
   * 
   * @serial
   */
  protected CommandAreaControl _cmdAreaPanel;
 
  /**
   * Variable for the toolbar control
   * 
   * @serial
   */
  protected ToolBarControl _tbc;
 
  /**
   * Boolean variable that is set if this is a dialog.
   * 
   * @serial
   */
  protected boolean _isDialog;
 
  /**
   */
  protected Dimension _size;

  /**
   * External access to JFrame reference may be needed 
   * for defining proper parent for a dialog, etc.  SMCook added.
   */
  protected JFrame _jframe;

  /**
   * A list of open WindowControls.  SMCook added.
   */
  protected static Vector _winVect = new Vector();

  /**
   * Constructs a new window control. The control is a frame
   * control.
   */
  public WindowControl(int id) {
    this(id, false, false);
  }

  /**
   * Constructs a new window control. The window control
   * can be a frame or a dialog depending on the boolean 
   * variable.
   * 
   * @param isDialog   boolean variable that is <code>true</code> if this 
   *           control is a dialog; <code>false</code> otherwise
   */
  public WindowControl(int id, boolean isDialog, boolean isModal) {
    _id = id; 
    _isDialog = isDialog;
    
    int parentId = ButtonPressActionHandler.getMostRecentWindowId();

    if(false) {
      Console.logMessage(this, "id       = " + id);
      Console.logMessage(this, "isDialog = " + isDialog);
      Console.logMessage(this, "isModal  = " + isModal);
      Console.logMessage(this, "parentId = " + parentId);
      Console.logMessage(this, "vectsize = " + _winVect.size());
    }

    if ( _isDialog ) {
      JFrame firstJFrame = AppGUIController._firstJFrame;

      if(parentId >= 1) {
        process(new JDialog(getJFrame(parentId), isModal));
      }
      else if(firstJFrame != null) {
        process(new JDialog(        firstJFrame, isModal));
      }
      else {
        process(new JDialog(new JFrame("dummy"), isModal));
      }
    }
    else {
      _jframe = new JFrame();  //SMCook, retain reference
      process(_jframe);  
    }
  }

  /**
   * Private access to all previous _jframe's. 
   */
  private JFrame getJFrame(int id) {
    int imax = _winVect.size();
    for(int i=0; i<imax; i++) { 
      WindowControl win = (WindowControl)_winVect.elementAt(i);
      if(win == null) {
        continue;
      }
      else if(win.getWindowID() == id) {
        JFrame tmp = win.getJFrame();
        if(tmp == AppGUIController.getTopLevelWindow(_id).getJFrame()) {
          Console.logMessage(this, "tmp matches topLevel");
        }
        else {
          Console.logMessage(this, "tmp does not match topLevel");
        }
        return tmp;
      }
    }
    return null;
  }

  /**
   * Public access to windowID for this control.
   */
  public int getWindowID() {
    return _id;
  }

  /**
   * Public access to 'this' _jframe. 
   */
  public JFrame getJFrame() {
    return _jframe;
  }

  /**
   * Protected method that is called by the constructor. The
   * window object passed as an argument can be a frame or a dialog.
   * 
   * @param w the window object 
   */
  protected void process(Window w) {
    _frame = w;

    _winVect.addElement(this); 

    _windowListener = new WindowVetoer();
    _cmdAreaPanel = new CommandAreaControl();
    _topPanel = new JPanel(new BorderLayout());
    _topPanel.add(_cmdAreaPanel.getComponent(), BorderLayout.CENTER);

    _frame.setBackground(_flickerSuppressionColor);     //SMCook
    _topPanel.setBackground(_flickerSuppressionColor);
    _cmdAreaPanel.getComponent().setBackground(_flickerSuppressionColor);

    getContentPane().add(_topPanel);
    _frame.addWindowListener(_windowListener);
  }
 
  /**
   */
  public boolean isDialog() {
    return _isDialog;
  }
 
  /**
   * Returns the GUI component being encapsulated by this control. 
   * This method will be invoked typically by a GUI builder for adding
   * this control to the GUI.
   * 
   * @return the GUI component contained by this control
   */
  public Component getComponent() {
    return _frame;
  }

  /**
   * Sets the menubar on this window. 
   * 
   * @param menuBar the menubar to be set on this window
   */
  public void setMenuBar(MenuBarControl menuBar) {
    _menuBarControl = menuBar;
    if (_isDialog) {
      ((JDialog) _frame).setJMenuBar((JMenuBar) menuBar.getComponent());
    } 
    else {
      ((JFrame) _frame).setJMenuBar((JMenuBar) menuBar.getComponent());
    }
  }
 
  /**
   * Returns the menu bar control.
   * 
   * @return the menu bar control set on this window control
   */
  public MenuBarControl getMenuBar() {
    return _menuBarControl;  
  }
 
  /**
   * Returns the panel component that contains the 
   * toolbar control.
   * 
   * @return   the panel component that contains the
   *       toolbar control
   */
  public JPanel getTopPanel() {
    return _topPanel;  
  }    
 
  /**
   * Posts a request for obtaining focus. 
   * 
   * @param rowNumber this parameter is applicable only to array and arrayset
   *           controls; fields and other controls disregard this parameter
   */
  public void requestFocus(int rowNumber) {
    _frame.requestFocus();
  }
 
  /**
   * Adds a GUI control with the specified constraints.
   * 
   * @param control the GUI control to be added to this window
   * @param constraint   the constraints that are to be used
   *             for adding the control
   */
  public void add(GUIControl control, Object constraint) {
    if ( control instanceof ToolBarControl ) {
      _tbc = (ToolBarControl) control;
      _topPanel.add(control.getComponent(), constraint);
    } 
    else {
      _cmdAreaPanel.add(control, constraint);
    }
  }
 
  /**
   * Sets the position of the help tip panel.
   * 
   * @param position   the position that describes the help tip
   *           panel positioning; possible values are "top"
   *           and "bottom"
   */
  public void setHelpPanelPosition(String position) {
    _cmdAreaPanel.setHelpPanelPosition(position);
  }  

  /**
    * Method that sets the presence of the top area component for this
    * window control. 
   * 
   * @param b   the boolean variable that is set if this window control
   *     is to have a top area component
    */
  public void setTopAreaComponentPresent(boolean b) {
    if (b) {
      _cmdAreaPanel.activateTopArea();
    }
  }
 
  /**
   */
  public void setSize(Dimension d) {
    //_size = d;   //pre-1.4 logic
    _size = new Dimension(
      (int)(WINDOW_WSCALE_FACTOR * d.width),
      (int)(WINDOW_HSCALE_FACTOR * d.height));  //SMCook
  }
 
  /**
   */
  public void setVisible(boolean b) {
    _frame.pack();
    if ( _size != null ) {
      _frame.setSize(_size);
    }
    Dimension d1 = Toolkit.getDefaultToolkit().getScreenSize();
    Dimension d2 = _frame.getSize();

    if(d1.width > 1900) d1.width /= 2;   //SMCook - for dual-screen quirk

    int ix = (d1.width - d2.width);
    int iy = (d1.height - d2.height);

    _frame.setLocation(ix/2,iy/8);
    _frame.setVisible(true);  
  }

  /**
    * Method that sets the presence of the bottom area component for this
    * window control. 
   * 
   * @param b   the boolean variable that is set if this window control
   *     is to have a bottom area component
    */
  public void setBottomAreaComponentPresent(boolean b) {
    if ( b ) {
      _cmdAreaPanel.activateBottomArea();
    }
  }    
 
  /**
   * Returns the container that contains the top area component
   * of this window control.
   * 
   * @return   the container containing the top area component; 
   *     <code>null</code> if top area component is not present
   */
  public JPanel getTopComponentPanel() {
    return _cmdAreaPanel.getTopComponentPanel();  
  }

  /**
   * Returns the container that contains the bottom area component
   * of this window control.
   * 
   * @return   the container containing the bottom area component; 
   *     <code>null</code> if bottom area component is not present
   */
  public JPanel getBottomComponentPanel() {
    return _cmdAreaPanel.getBottomComponentPanel();    
  }
 
  /**
   * Sets the JPanel that contains the help tip.  //SMCook
   */
  public void setHelpPanel(JPanel p) {
    _cmdAreaPanel.setHelpPanel(p);
  }
 
  /**
   * Returns the panel component that contains the help.
   * 
   * @return the panel component containing the help
   */
  public JPanel getHelpPanel() {
    return _cmdAreaPanel.getHelpPanel();
  }
 
  /**
   * Sets the title on this window control.
   * 
   * @param title the title to be set on this window control
   */
  public void setTitle(String title) {
    if (_isDialog) {
      ((JDialog) _frame).setTitle(title);  
    } 
    else {
      ((JFrame) _frame).setTitle(title);  
    }
  }
 
  /**
   * Returns the title of this window control.
   * 
   * @return the title of this window control
   */
  public String getTitle() {
    if (_isDialog) {
      return ((JDialog) _frame).getTitle();  
    } 
    else {
      return ((JFrame) _frame).getTitle();  
    }
  }
 
  /**
   * Returns the content pane of this window.
   * 
   * @return the content pane of this control
   */
  public Container getContentPane() {
    if (_isDialog) {
      return ((JDialog) _frame).getContentPane();  
    } 
    else {
      return ((JFrame) _frame).getContentPane();  
    }
  }
 
  /**
   * Sets the content pane on this window.
   * 
   * @param c the content pane to be set on this window
   */
  public void setContentPane(Container c) {
    if (_isDialog) {
      ((JDialog) _frame).setContentPane(c);  
    } 
    else {
      ((JFrame) _frame).setContentPane(c);  
    }
  }
 
  /**
   * Removes all the components from thsi window control.
   */
  public void removeAll() {
    _frame.removeAll();  
  }
 
  /**
   * Posts a request to for repaint on the event dispatching 
   * thread.
   */
  public void repaint() {
    _frame.repaint();  
  }
 
  /**
   * Invalidates this control.
   */
  public void invalidate() {
    _frame.invalidate();  
  }

  /**
   * Validates this control.
   */
  public void validate() {
    _frame.validate();  
  }
 
  /**
   * Disposes this window.
   */
  public void dispose() {
    //Console.logMessage(this, "dispose");
    removeFromWindowVect();  //SMCook

    _frame.removeWindowListener(_windowListener);
    if (_tbc != null ) {
      _tbc.dispose();  
    }
    _cmdAreaPanel.dispose();
    ((Container) _cmdAreaPanel.getComponent()).removeAll();
    _topPanel.removeAll();
    _frame.removeAll();
    if(_isDialog) { 
      if (((JDialog) _frame).getJMenuBar() != null) {
        JRootPane rp = ((JDialog) _frame).getRootPane();
        JLayeredPane lp = rp.getLayeredPane();
        lp.remove(rp.getJMenuBar());
      }
    } 
    else {
      if (((JFrame) _frame).getJMenuBar() != null) {
        JRootPane rp = ((JFrame) _frame).getRootPane();
        JLayeredPane lp = rp.getLayeredPane();
        lp.remove(rp.getJMenuBar());
      }
    }
    _frame.setVisible(false);
    _frame.dispose();
    super.dispose();
  }

  /**
   * This method is intended to prevent garbage from accumulating.
   *
   * @param e the event generated
   */
  public void removeFromWindowVect() {
    int imax = _winVect.size();
    for(int i=0; i<imax; i++) { 
      WindowControl win = (WindowControl)_winVect.elementAt(i);
      if(win == null)
        continue;
      else if(win.getWindowID() == _id) {
        //Console.logMessage(this, "removing " + _id + " from open list");
        _winVect.removeElementAt(i);
        break;
      }
    }
  }

 
  /**
   * Inner class that sends window focus notifications to GUI listeners.
   */
  class WindowVetoer extends WindowAdapter {
    /**
     * This method is activated when the window control is activated.
     * 
     * @param e the event that is generated when this window control
     *       is activated
     */
    public void windowDeactivated(WindowEvent e) {
      time1 = System.currentTimeMillis();
    }

    public void windowActivated(WindowEvent e) {
      // SMCook workaround code ********
      //  for client-server 1.4 Windows version wherein
      // windows wouldn't maintain their proper layering.
      //if(System.currentTimeMillis() - time1 < 400) {
      //  if(getTitle().indexOf("CFE") == 0) {
      //    e.getOppositeWindow().toFront();
      //    return;
      //  }
      //}
      // end workaround **********

      try {
        GUIControlEvent event = new 
          GUIControlEvent(WindowControl.this, 
            GUIControlEvent.WINDOW_FOCUS_EVENT, null);

        fireGUIControlChanged(event);
      }
      catch (NullPointerException ex) {
        System.err.println(
          "WindowControl: windowActivated: NullPointerException");  
      }
      catch (GUIControlException ex) {
        System.err.println(
          "WindowControl: windowActivated: GUIControlException");  
      }
    }

    /**
     * This method is activated when the window is requested to be closed
     * by the user.
     *
     * @param e the event generated
     */
    public void windowClosing(WindowEvent e) {
      try {
        GUIControlEvent event = new GUIControlEvent(WindowControl.this,
          GUIControlEvent.WINDOW_CLOSING_EVENT, null);

        fireGUIControlChanged(event);
      }
      catch (NullPointerException ex) {
        System.err.println(
          "WindowControl: windowClosing: NullPointerException");
      }
      catch (GUIControlException ex) {
        System.err.println(
          "WindowControl: windowClosing: GUIControlException");
      }
    }      
  }
 
  // for testing
  public static void main(String[] args) {
    WindowControl wc = new WindowControl(-1);
    wc.setTitle("WINDOW");
    wc.getComponent().setSize(300,300);
    wc.getComponent().setVisible(true);
  }
}
