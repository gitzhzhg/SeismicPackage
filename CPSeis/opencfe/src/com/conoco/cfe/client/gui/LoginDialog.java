///
/// LoginDialog.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  7.
///  6. 10-08-2001 SMCook   Removed LOGIN_WAS_USED variable.  Gave task of
///                          reporting whether program is running in
///                          client/server mode to class Application (more
///                          natural grouping of tasks).
///  5. 09-26-2001 SMCook   Handled dual-screen centering quirk (preemptive).
///  4. 09-24-2001 SMCook   Changed layout scheme and dialog sizes to improve
///                          appearance.
///                         Report username (so cfecustom can work remotely).
///                         Added static LOGIN_WAS_USED variable to allow other
///                          classes to easily find out if CFE is running in
///                          client-server mode.
///

package com.conoco.cfe.client.gui;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Application;

import com.conoco.cfe.client.messaging.UserValidator;
import com.conoco.cfe.client.messaging.HTTPUserValidator;
import com.conoco.cfe.client.messaging.Preferences;

import com.conoco.cfe.utils.EventQueue;

import java.awt.BorderLayout;
import java.awt.Event;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.Dimension;
import java.awt.Cursor;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import java.net.MalformedURLException;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JPasswordField;
import javax.swing.JPanel;
import javax.swing.Timer;
import javax.swing.KeyStroke;
import javax.swing.text.Keymap;

/**
 * A dialog for user login. The login dialog is
 * shown in the very beginning of the application. The 
 * dialog communicates with a user name validator 
 * which in turn communicates with the server for 
 * login validation. The login validation is handled 
 * by a servlet residing on the web server. The login servlet
 * URL is specified in the preferences file.
 * 
 * @see com.conoco.cfe.messaging.UserValidator
 * @see com.conoco.cfe.messaging.UserValidationCommModule
 * @see com.conoco.cfe.servlet.LoginServlet
 * @see preferences.dtd
 * @see prefs.xml
 */
public class LoginDialog extends JDialog {

  /**
   * Constant for the dialog width
   * 
   * @serial
   */
  private static final int WIDTH = 450;
  
  /**
   * Constant for the dialog height
   * 
   * @serial
   */
  private static final int HEIGHT = 250;
  
  /**
   * Static initializer to remove the default 'enter' key binding.
   */
  static {
    JPasswordField f = new JPasswordField();
    KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
    Keymap map = f.getKeymap();
    map.removeKeyStrokeBinding(enter);
  }  
  
  /**
   * Variable for the "Ok" button.
   * 
   * @serial
   */  
  protected JButton _ok;
  
  /**
   * Variable for the "Cancel" button
   * 
   * @serial
   */
  protected JButton _cancel;
  
  /**
   * Variable for the "Reset" button
   * 
   * @serial
   */
  protected JButton _reset;
  
  /**
   * Variable for the "Login" label
   * 
   * @serial
   */
  protected JLabel _loginLabel;
  
  /**
   * Variable for the "Password" label
   * 
   * @serial
   */
  protected JLabel _passLabel;
  
  /**
   * Variable for the "Login" field
   * 
   * @serial
   */
  protected JTextField _login;
  
  /**
   * Variable for the "Password" field
   * 
   * @serial
   */
  protected JPasswordField _pass;
  
  /**
   * Variable for the user name validator
   * 
   * @serial
   */
  protected UserValidator _validator;
      
  /**
   * Variable for the message dialog
   */
  protected JDialog _messageDialog;


  /**
   * Let username be accessible.
   */
  public static String username;


  /**
   * Constructs a new login dialog with the 
   * specified title.
   * 
   * @param title the login dialog title
   */  
  public LoginDialog(String title) {
    super();
    setTitle(title);
    setModal(true);
    
    initializeComponents();  
    addComponents();
    installListeners();
    
    setSize(new Dimension(WIDTH, HEIGHT));
    setResizable(false);
    setUpLoginValidator();
  }  
      
  /**
   * Initializes the components used by this dialog. This is 
   * a protected method called by the constructor.
   */
  protected void initializeComponents() {
    _loginLabel = new JLabel(   "Login:  ", JLabel.RIGHT);
    _passLabel  = new JLabel("Password:  ", JLabel.RIGHT);
    
    _pass = new JPasswordField(30);
    _login = new JTextField(30);    

    Font courier = new Font("Courier", Font.PLAIN, 12);
    _login.setFont(courier);
    _pass.setFont(courier);
    
    _ok     = new JButton("Ok");
    _cancel = new JButton("Cancel");
    _reset  = new JButton("Reset");
  }  
  
  /**
   * Sets up the URL for the servlet that will be used by the 
   * validator for validating the login. This is a protected
   * method called by the constructor.
   */
  protected void setUpLoginValidator() {
    _validator = new HTTPUserValidator();
    
    if ( !(_validator.init()) ) {
      String[] messages = { "LoginDialog: Malformed login servlet" };
      ErrorInfoWarningDialog.showErrorDialog(messages);
      System.exit(1);
    }  
  }
  
  /**
   * Adds the various components to this dialog.
   */
  protected void addComponents() {
    getContentPane().setLayout(new BorderLayout());

    JPanel logPanel=new JPanel(new FlowLayout(FlowLayout.RIGHT));
    logPanel.add(_loginLabel);
    logPanel.add(_login);

    JPanel passPanel=new JPanel(new FlowLayout(FlowLayout.RIGHT));
    passPanel.add(_passLabel);
    passPanel.add(_pass);

    JPanel subPan1=new JPanel(new GridLayout(2,1,0,25));
    subPan1.add(logPanel);
    subPan1.add(passPanel);

    JPanel subPan2=new JPanel(new FlowLayout(FlowLayout.CENTER,50,35));
    subPan2.add(subPan1);

    JPanel bottom = new JPanel(new FlowLayout(FlowLayout.CENTER,60,25));
    bottom.add(_ok);
    bottom.add(_cancel);
    bottom.add(_reset);
    
    getContentPane().add("Center",subPan2);
    getContentPane().add("South",bottom);
  }
    
  /**
   * Installs the listeners that listen to the buttons on this
   * dialog. This is a protected method called by the constructor.
   */
  protected void installListeners() {
    EnterAction enterAction = new EnterAction();
    
    _pass.registerKeyboardAction( enterAction, "Enter",
      KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0),
      JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    
    _cancel.addActionListener(new CancelListener());
    _ok.addActionListener( new OkListener());
    _reset.addActionListener( new ResetListener());
  }
  
  /**
   * Sets the visibility of this dialog. This method is overriden 
   * here to adjust the location of the dialog so that its 
   * at the center of the screen.
   * 
   * @param b the boolean variable specifying the visibility
   */
  public void setVisible(boolean b) {
    if ( _validator != null ) {
      Toolkit tk = Toolkit.getDefaultToolkit();
      Dimension d = tk.getScreenSize();
      if(d.width > 1900) d.width /= 2;
      setLocation((d.width - LoginDialog.WIDTH)/2, (d.height - LoginDialog.HEIGHT)/3);
      super.setVisible(true);  
      _login.requestFocus();
    } 
    else {
      System.exit(1);
    }
  }

  /**
   * Inner class that listens to the "Cancel" button.
   */
  protected class CancelListener implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      LoginDialog.this.dispose();
      System.exit(1);
    }  
  }
  
  /**
   * Inner class that listens to the "Reset" button.
   */
  protected class ResetListener implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      _login.setText(""); _login.invalidate(); _login.repaint();   // SMCook fix
      _pass.setText("");  _pass.invalidate();  _pass.repaint();    // sluggish
                          _reset.invalidate(); _reset.repaint();   // redraw

      _login.requestFocus();
    }

  }
  
  /**
   * Inner class that listens to the "Ok" button.
   */
  protected class OkListener implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      boolean auth = true;
      _validator.setUser(_login.getText());
      _validator.setPassword(new String(_pass.getPassword()));
      
      _messageDialog = new JDialog();
      _messageDialog.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
      _messageDialog.setTitle("Please wait....");
      
      _messageDialog.setModal(false);
      _messageDialog.getContentPane().setLayout(new BorderLayout());
      
      JPanel panel = new JPanel(new BorderLayout());

      _messageDialog.getContentPane().add(panel, BorderLayout.CENTER);

      JLabel label = new JLabel("Establishing connection to server",JLabel.CENTER);
      panel.add(label, BorderLayout.CENTER);

      _messageDialog.setSize(500, 100);

      Toolkit tk = Toolkit.getDefaultToolkit();
      Dimension dim = tk.getScreenSize();
      if(dim.width > 1900) dim.width /= 2;    //SMCook dual-screen quirk

      _messageDialog.setLocation((dim.width - 500)/2, (dim.height - 100)/3);
      _messageDialog.setVisible(true);

      Timer t = new Timer(500, new Validate());
      t.setRepeats(false);
      t.start();
    }  
  }
  
  private class Validate implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      String response = _validator.login();
      System.err.println(response);
      _messageDialog.dispose();
      _messageDialog = null;

      if ( response.startsWith("success") ) {
        LoginDialog.this.dispose();
      } 
      else {
        String[] messages = { "LoginDialog: User Authorization Failed", response };
        _reset.doClick();
        ErrorInfoWarningDialog.showErrorDialog(messages);
      }
    }
  }

  /**
   * Inner class that listens to the "Return" key action on 
   * the password field. When "Return" key is pressed, login
   * information is sent to the server.
   */
  protected class EnterAction implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      _ok.doClick();
      username = _login.getText();
    }
  }

  // for testing
  public static void main(String args[]) {
    LoginDialog d = new LoginDialog("Login");
    d.setVisible(true);  
  }
}
