///
/// ErrorInfoWarningDialog.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
/// 10. 10-17-2003 SMCook   Changed spot where convenience function for
///                          showInfoDialog was inadvertently calling
///                          showErrorDialog.  Cleaned up extraneous spaces
///                          left over long ago from detab.
///  9. 10-08-2003 SMCook   Changed constructors to allow for more combinations
///                          of modality and ability to omit buttons.
///  8. 09-29-2003 SMCook   Fixed error in overrided setVisible call wherein
///                          super.setVisible() was called with a true
///                          argument regardless of the value of the input
///                          argument to setVisible().  This was causing the
///                          long-standing problem of error windows popping up
///                          as the parent window was closed.  This problem
///                          goes all the way back to the original source code
///                          from INT.
///  7. 10-08-2001 SMCook   Now queries Application rather than LoginDialog to
///                          find out whether program is running client/server.
///  6. 09-26-2001 SMCook   Handled dual-screen centering quirk (preemptive).
///  5. 09-24-2001 SMCook   Added KILL CFE button when running in client-server
///                          and if error (not info or warning).
///

package com.conoco.cfe.client.gui;

import com.conoco.cfe.client.ClientConstants;
import com.conoco.cfe.client.Client;

import com.conoco.cfe.client.application.Application;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Dimension;
import java.awt.Toolkit;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.util.StringTokenizer;

import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JPanel;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JTextArea;

/**
 * A dialog component that is displayed by the client when it receives
 * an error, info or warning message from the server. The actual
 * dialog displayed depends upon the type which by default is ERROR. 
 * Possible values for the type are ERROR, INFO and WARNING.
 */
public class ErrorInfoWarningDialog
  extends JDialog
  implements ActionListener {
 
  /**
   * Constant that identifies that this is an error message
   * 
   * @serial
   */
  public final static int ERROR = 0;
 
  /**
   * Constant that identifies that this is an info message
   * 
   * @serial
   */
  public final static int INFO = 1;
 
  /**
   * Constant that identifies that this is an warning message
   * 
   * @serial
   */
  public final static int WARNING = 2;
 
  /**
   * Declares a variable for type of dialog. Possible 
   * values are ERROR, INFO and WARNING.
   * 
   * @serial
   */
  protected int _type;
 
  /**
   * Declares a variable for the scroll pane that will contain
   * the panels added to the dialog.
   * 
   * @serial
   */
  protected JScrollPane _scrollPane;
 
  /**
   * Variable for the text area component
   * 
   * @serial
   */
  protected JTextArea _area;
 
  /**
   * Button panel and buttons
   * 
   * @serial
   */
  protected JPanel _buttonPanel;
  protected JButton _okButton;
  protected JButton _killAppButton;
  protected boolean _includeButtons = true;


  /**
   * Constructs a new dialog using unspecified parent.  This is generally
   * undesirable because dialog can then get covered by other windows.
   */
  public ErrorInfoWarningDialog() {
    this(new JFrame("Untitled"));
  }

  /**
   * Constructors for a new dialog using specific parent.
   * Highly desirable if modal behavior is being used.
   * 
   * @param f the parent of this dialog
   */
  public ErrorInfoWarningDialog(JFrame f) {
    this(f, true, true);
  }

  /**
   * Constructor for dropping buttons.
   * 
   * @param f the parent of this dialog
   * @param includeButtons whether to include the buttons.
   */
  public ErrorInfoWarningDialog(
    JFrame f, boolean isModal, boolean includeButtons) {

    super(f, isModal);
    _includeButtons = includeButtons;
    init();
  }

  /**
   * Protected method called by the contructors. Initializes the 
   * components used by this dailog.
   */
  protected void init() {
    Client.FOCUS_FLAG = true;
    _buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER,100,5));
    _area = new MyTextArea(5, 30);
    _area.setEditable(false);
    _area.setFont(ClientConstants.getDefaultFieldFont());

    _scrollPane = new JScrollPane(_area);
    ((JPanel) getContentPane()).setPreferredSize(new Dimension(400, 150));
    getContentPane().add(_scrollPane, BorderLayout.CENTER);

    _okButton = new JButton("Ok");
    _okButton.addActionListener(this);

    _killAppButton = new JButton("KILL CFE");
    _killAppButton.addActionListener(this);

    if(_includeButtons) {
      _buttonPanel.add(_okButton);
      if(Application.isClientServer()) _buttonPanel.add(_killAppButton);
      getContentPane().add(_buttonPanel, BorderLayout.SOUTH);
    }
  }
 
  /**
   * Sets the type of this dialog.
   * 
   * @param type the type of this dialog
   */
  public void setType(int type) {
    _type = type;
    if ( _type == 0 ) {
      setTitle("Error");
      _buttonPanel.remove(_killAppButton);
      if(Application.isClientServer()) {
        _buttonPanel.add(_killAppButton);
      }
    } 
    else if ( _type == 1 ) {
      setTitle("Info");
      _buttonPanel.remove(_killAppButton);
    } 
    else if ( _type == 2 ) {
      setTitle("Warning");
      _buttonPanel.remove(_killAppButton);
    }
  }

  /**
   * Returns the type of this dialog.
   * 
   * @return the type of this dialog
   */
  public int getType() {
    return _type;
  }

  /**
   * Sets the messages to be displayed by this dialog.
   * 
   * @param messages the messages to be shown
   */
  public void setMessages(String[] messages) {
    for ( int i=0; i<messages.length; i++ ) {
      _area.append(messages[i]);
      _area.append("\n");  
    }
  }
 
  /**
   * Sets the visibility of this dialog. This method is overriden 
   * here to adjust the location of the dialog so that its 
   * at the center of the screen.
   * 
   * @param b the boolean variable specifying the visibility
   */
  public void setVisible(boolean b) {
    if(!b) {
      super.setVisible(false);
      return;
    }
    pack();
    Toolkit tk = Toolkit.getDefaultToolkit();
    Dimension d = tk.getScreenSize();
    if(d.width > 1900) d.width /= 2;  //SMCook - dual-screen quirk

    Dimension size = new Dimension(500, 400);
    setSize(size);
    setLocation((d.width - size.width)/2, (d.height - size.height)/3);
    super.setVisible(true);  
  }

  /**
   * Converts the message text into HTML formatted text. 
   * 
   * @param s the string in plain text to be formatted
   * @return the HTML formatted text
   */
  protected String convertToHTML(String s) {
    StringTokenizer tokenizer = new StringTokenizer(s, " ");
    int count = 0;
    StringBuffer buf = new StringBuffer();
    buf.append("<html>");
    //buf.append("<body>");
    while (tokenizer.hasMoreTokens()) {
      ++count;
      //System.out.println("token - " + tokenizer.nextToken());  
      buf.append(tokenizer.nextToken());
      buf.append(" ");
      if ( (count % 5) == 0 ) {
        buf.append("<br>");  
      }
    }
    //buf.append("</body>");
    //buf.append("</html>");  
    //System.out.println("html " + buf.toString());
    return buf.toString();
  }
 
  /**
   * Convenience methods to allow for easy creation of ErrorDialogs
   * The second is normally preferable.
   */
  public static void showErrorDialog(String [] messages) {
    showErrorDialog(messages,new JFrame());
  }
  public static void showErrorDialog(String [] messages,JFrame f) {
    ErrorInfoWarningDialog d = new ErrorInfoWarningDialog(f);
    d.setType(ErrorInfoWarningDialog.ERROR);
    d.setMessages(messages);
    d.setSize(200,200);
    MyThread t = new MyThread(d);
    t.start();
  }

  /**
   * Convenience methods to allow for easy creation of ErrorDialogs
   * The second is normally preferable.
   */
  public static void showInfoDialog(String [] messages) {
    showInfoDialog(messages,new JFrame());
  }
  public static void showInfoDialog(String [] messages,JFrame f) {
    ErrorInfoWarningDialog d = new ErrorInfoWarningDialog(f);
    d.setType(ErrorInfoWarningDialog.INFO);
    d.setMessages(messages);
    d.setSize(200,200);
    MyThread t = new MyThread(d);
    t.start();
  }

  /**
   * Convenience methods to allow for easy creation of ErrorDialogs
   * The second is normally preferable.
   */
  public static void showWarningDialog(String [] messages) {
    showWarningDialog(messages,new JFrame());
  }
  public static void showWarningDialog(String [] messages,JFrame f) {
    ErrorInfoWarningDialog d = new ErrorInfoWarningDialog(f);
    d.setType(ErrorInfoWarningDialog.WARNING);
    d.setMessages(messages);
    d.setSize(200,200);
    MyThread t = new MyThread(d);
    t.start();
  }

  /**
   * Convenience method to allow non-modal creation of ErrorDialogs
   */
  public static void showNonmodalWarningDialog(String [] messages) {
    showNonmodalWarningDialog(messages,new JFrame());
  }
  public static void showNonmodalWarningDialog(String [] messages,JFrame f) {
    ErrorInfoWarningDialog d = new ErrorInfoWarningDialog(f);
    d.setType(ErrorInfoWarningDialog.WARNING);
    d.setMessages(messages);
    d.setSize(200,200);
    d.setVisible(true);
  }

  /**
   * Catch action events from button(s).
   * The dialog is disposed when the user clicks either button.
   * If the "Kill CFE" button is pressed,
   */
  public void actionPerformed(ActionEvent e) {
    Object src=e.getSource();
    if(src == _okButton) {
      Client.FOCUS_FLAG = false;
      ErrorInfoWarningDialog.this.dispose();
      Client.RESET_TIMEOUT = true;
      return;
    }
    else if(src == _killAppButton) {
      System.exit(1);
    }
  }
 
  /**
   * Inner class that extending a text area component to support
   * word wrapping.
   */
  private class MyTextArea extends JTextArea {
    public MyTextArea() {
      super();
    }

    public MyTextArea(int r, int c) {
      super(r, c);
    }

    public boolean getLineWrap() {
      return true;
    }  

    public boolean getWrapStyleWord() {
      return true;
    }
  }
 
  /**
   * Inner class that is used to create a new thread to 
   * set the dialog visible if it is modal. This is required 
   * because we want the event dispatching thread to process
   * all the pending events and then show the modal dialog.
   */
  private static class MyThread extends Thread {
    ErrorInfoWarningDialog _d;

    public MyThread(ErrorInfoWarningDialog d) {
      _d = d;
    }

    public void run() {
      _d.setVisible(true);
    }
  }
 
  /**
   * This is here for testing.
   */
  public static void main(String[] args) {
    ErrorInfoWarningDialog d = new ErrorInfoWarningDialog();
    d.setType(INFO);
    String[] messages = { "jshdjdskjsadkjsks fkf fkkf fkfk fkfk "
      + " jksdfbjsdf oksdfksdjfklds o fdlkjds ;ldsfkl;dskfl;dskf " + 
        " ksaklsajfdksjfkds klfd lkdf lkdf l fksl;dskf", "osjahdd", "AKJD",
          "OKOKOKO", "osososos", "iiiiiiii", "oksooooowwowo"};
    d.setMessages(messages);
    d.setVisible(true);
  }
}
