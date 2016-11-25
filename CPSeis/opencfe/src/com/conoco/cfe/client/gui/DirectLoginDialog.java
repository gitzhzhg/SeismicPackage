// DirectLoginDialog.java

package com.conoco.cfe.client.gui;

import com.conoco.cfe.client.messaging.UserValidator;
import com.conoco.cfe.client.messaging.HTTPUserValidator;
import com.conoco.cfe.client.messaging.Preferences;

import com.conoco.cfe.utils.EventQueue;

import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.Dimension;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.net.MalformedURLException;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JFrame;
import javax.swing.JTextField;
import javax.swing.JPasswordField;
import javax.swing.JPanel;
import javax.swing.Timer;
import javax.swing.KeyStroke;
import javax.swing.text.Keymap;


/**
 * A login dialog for user validation that is
 * used by <code>com.conoco.cfe.client.application.DirectApplication</code>.
 * Current implementation does not perform any validation.
 *
 * @see com.conoco.cfe.client.application.DirectApplication
 */
public class DirectLoginDialog extends JDialog {
  
  /**
   * Constant for the dialog width
   * 
   * @serial
   */
  private static final int WIDTH = 400;
  
  /**
   * Constant for the dialog height
   * 
   * @serial
   */
  private static final int HEIGHT = 220;
  

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
   * Variable for the message dialog
   */
  protected JDialog _messageDialog;

  /**
   * Constructs a new login dialog with the 
   * specified title.
   * 
   * @param title the login dialog title
   */  
  public DirectLoginDialog(String title) {
    super();
    setTitle(title);
    setModal(true);
    
    initializeComponents();  
    addComponents();
    installListeners();
    setSize( new Dimension(WIDTH, HEIGHT));
    setResizable(false);
    setUpLoginValidator();
  }  
      
  /**
   * Initializes the components used by this dialog. This is 
   * a protected method called by the constructor.
   */
  protected void initializeComponents() {
    _loginLabel = new JLabel("Login");
    _passLabel = new JLabel("Password");
    
    _pass = new JPasswordField(15);
    _login = new JTextField(15);    

    Font courier = new Font("Courier", Font.PLAIN, 12);
    _login.setFont(courier);
    _pass.setFont(courier);
    
    _ok = new JButton("Ok");
    _cancel = new JButton("Cancel");
    _reset = new JButton("Reset");
  }  
  
  /**
   * Sets up the URL for the servlet that will be used by the 
   * validator for validating the login. This is a protected
   * method called by the constructor.
   */
  protected void setUpLoginValidator() {
  }
  
  /**
   * Adds the various components to this dialog.
   */
  protected void addComponents() {
    GridBagLayout l = new GridBagLayout();
    GridBagConstraints c = new GridBagConstraints();
    
    getContentPane().setLayout(l);
    
    c.weighty = 1.0;
    c.weightx = 1.0;    
    l.setConstraints(_loginLabel, c);
    getContentPane().add(_loginLabel);
    
    c.gridx = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    l.setConstraints(_login, c);
    getContentPane().add(_login);
    
    c.gridx = 0;
    c.gridy = 1;
    c.fill = GridBagConstraints.NONE;
    l.setConstraints(_passLabel, c);
    getContentPane().add(_passLabel);
    
    c.gridx = 1;
    c.fill = GridBagConstraints.HORIZONTAL;
    l.setConstraints(_pass, c);
    getContentPane().add(_pass);
    
    JPanel bottom = new JPanel(new GridLayout(1,3));
    bottom.add(_ok);
    bottom.add(Box.createGlue());
    bottom.add(_cancel);
    bottom.add(Box.createGlue());
    bottom.add(_reset);
    
    c.gridwidth = 2;
    c.gridx = 0;
    c.gridy = 2;
    l.setConstraints(bottom, c);
    getContentPane().add(bottom);
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
    Toolkit tk = Toolkit.getDefaultToolkit();
    Dimension d = tk.getScreenSize();
    Dimension size = getSize();
    setLocation((d.width - size.width)/2, (d.height - size.height)/2);
    super.setVisible(true);
  }

  /**
   * Inner class that listens to the "Cancel" button.
   */
  protected class CancelListener implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      //setVisible(false);
      dispose();
      System.exit(1);  
    }  
  }
  
  /**
   * Inner class that listens to the "Reset" button.
   */
  protected class ResetListener implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      _login.setText("");
      _pass.setText("");
      EventQueue.invokeLater(new MyRunnable());
    }  
    
    class MyRunnable implements Runnable {
      public void run() {  
        _login.requestFocus();
      }
    }
  }
  
  /**
   * Inner class that listens to the "Ok" button.
   */
  protected class OkListener implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      //DirectLoginDialog.this.setVisible(false);
      DirectLoginDialog.this.dispose();
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
    }
  }
  
  // for testing
  public static void main(String args[]) {
    DirectLoginDialog d = new DirectLoginDialog("Login");
    d.setVisible(true);  
  }
}