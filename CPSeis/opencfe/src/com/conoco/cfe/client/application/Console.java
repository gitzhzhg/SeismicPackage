///
/// Console.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  5.
///  4. 08-13-2002 SMCook   Added additional form of logMessage() that reports
///                          the name of the class that called it.  Useful for
///                          debugging.
///                         Made most lines of code adhere to an 80-char limit.
///

package com.conoco.cfe.client.application;

import com.conoco.cfe.utils.EventQueue;

import java.awt.Font;
import java.awt.Point;

import javax.swing.JFrame;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import javax.swing.JViewport;

import java.util.Properties;

/**
 * Impements the console window that can be used for logging 
 * client and server messages.
 */
public class Console {
   
  /**
   * Variable for the text area component
   * 
   * @serial
   */
  protected static JTextArea _area;
  
  /**
   * Variable for the frame component
   * 
   * @serial
   */
  protected static JFrame _frame;
  
  /**
   * Variable for the scroll pane
   * 
   * @serial
   */
  protected static JScrollPane _scrollPane;  
  
  /**
   * Prevents a console instance from being invoked, all messages
   * are sent to stdout ONLY
   *
   * false by default
   */
  private static boolean _console_off = false;
   
  /**
   * A boolean variable which is set to true if the messages
   * are to be printed to the standard output also
   *
   * @serial
   */
  protected static boolean _toStdOut = true;
  
  /**
   * A boolean variable that is set to true once a console
   * window has been instantiated
   * 
   * @serial
   */
  protected static Console _instance;
  
  /**
   * Constructor
   */
  public Console() {    
  }
  
  /**
   * used to get only one instance of console
   */
  static Console getConsoleInstance() {
    if( _instance == null ){
      _instance = new Console();
      getConsoleWindow();
     }
     return(_instance);
  }
   
  /**
   * Method callled by WithConsoleActionHandler. 
   */
  static public void turnOffConsoleWindow() {
    _console_off = true;
  }
   
  /**
   * Creates a console window. This method is a protected method 
   * called by the constructor or the <code>logMessage(java.lang.String)</code>
   * method. This metod gets called only once.
   */
  protected static void getConsoleWindow() {
    if((!_console_off) && (_instance != null)){
      _area = new JTextArea();
      _area.setEditable(false);
      _area.setFont(new Font("Courier", Font.PLAIN, 12));
      _frame = new JFrame("Console");
      _scrollPane = new JScrollPane(_area);
      _frame.getContentPane().add(_scrollPane);
      _frame.setSize(900, 200);
      _frame.setVisible(true);
      preventClassGarbageCollection();
    }
  }
  
  /**
   * Sets the boolean flag that controls the printing of messages
   * to the standard output.
   * 
   * @param b is <code>true</code> if printing to standard output is required
   */
  public static void setStandardOutput(boolean b) {
    _toStdOut = b;
  }
  
  /**
   * Returns a boolean variable that controls the printing to the standard 
   * output.
   * 
   * @return the boolean variable that controls the printing to the standard output
   */
  public static boolean isStandardOutput() {
    return _toStdOut;  
  }
 
  /**
   * Print amount of free memory to the console.  Run garbage collector first.
   */
  public static void logFreeMemory() {
    logFreeMemory(null);
  }

  /**
   * Print amount of free memory to the console.  Run garbage collector first.
   */
  public static void logFreeMemory(String s) {
    System.gc();
    if(s==null)
      logMessage(
        "Free memory is " + Runtime.getRuntime().freeMemory() + " bytes.");
    else
      logMessage(
        s + " Free memory is " + Runtime.getRuntime().freeMemory() + " bytes.");
  }
 
  /**
   * Logs a message to the console.  Version 1 of 2.
   * 
   * @param message the messages to be displayed in the console window
   */
  public static void logMessage(String message) {
    EventQueue.invokeLater(new LogRunnable(message));  
  }

  /**
   * Logs a message to the console.  Version 2 of 2 (prints object class name).
   * 
   * @param message the messages to be displayed in the console window
   */  
  public static void logMessage(Object obj, String message) {
    String s = obj.getClass().getName();
    int len = s.length();
    int pos = s.lastIndexOf(".") + 1;
    EventQueue.invokeLater(
      new LogRunnable(s.substring(pos, len) + ": " + message));  
  }
  
  /**
   * Inner class thats is used to post an asynchronous message
   * on the event dispatching thread. The event queue thread calls
   * the <code>run</code> method of this class once all the pending
   * events are processed.
   */
  static class LogRunnable implements Runnable {
    
    /**
     * Variable for storing the message
     * 
     * @serial
     */
    private String _msg;
    
    private Console _console = null;
    
    /**
     * Constructs a new message dispatcher object.
     * 
     * @param message the message to be displayed 
     */
    public LogRunnable(String message) {
      setMessage(message);  
    }
    
    /**
     * Sets the message that will be displayed by the message
     * logger.
     * 
     * @param message the message 
     */
    public void setMessage(String message) {
      _msg = message;  
    }
    
    /**
     * This method is invoked when the event dispatching thread 
     * processes this runnable object. It basically writes the
     * message to the console window. If the console frame
     * does not exist, a new one is first constructed.
     */
    public void run() {
      // create a new console instance if needed
        _console = getConsoleInstance();
             
      // continue only if console is enabled
      if(_console_off == false)  {
         if (!(_frame.isVisible())) {
            _frame.setVisible(true);
         }
      
         _area.append("\n" + _msg);      
         JViewport vp = _scrollPane.getViewport();
         vp.setViewPosition(new Point(
                            0, _area.getSize().height - vp.getHeight()));
      }
      
      // print out a message if required or if no console is used
      if (isStandardOutput() || (_console_off == true)) {
        System.out.println(_msg);  
      }
    }
  }
  
  /**
   * Prevents this class from getting garbage collected.
   */
  private static void preventClassGarbageCollection() {
       Properties systemTable = System.getProperties();
       systemTable.put( "_$com.conoco.cfe.client.application.Console.instance",
                                    getConsoleInstance() );
  }
}
