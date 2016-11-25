// XmlMessageBuilder.java

package com.conoco.cfe.server.messaging;

import com.conoco.xml.StringArray;
import com.conoco.xml.XML;

/**
 * This class is used by the CFE API to build reply XML messages to be sent to the
 * client.
 */
public class XmlMessageBuilder
  implements MessageBuilder, Runnable {

  /**
   * A static constant for the exit time period
   * 
   * @serial
   */
  static final int EXIT_TIMEOUT_LENGTH = 5000; // in milliseconds

  /**
   * A static constant for the inactivity timeout period
   * 
   * @serial
   */
  static final int DEFAULT_TIMEOUT_INTERVAL = 61 * 60 * 1000; // in milliseconds

  /**
   * Variable to hold the servlet timeout period
   * 
   * @serial
   */
  protected int _servletTimeout = 61 * 60 * 1000;
  
  /**
   * Variable for the XML node for the message
   * 
   * @serial
   */
  protected XML _message;
  
  /**
   * Variable for the XML node for the reply
   *
   * @serial
   */
  protected XML _reply;

  /**
   * Variable for the thread that controls the exit action
   * 
   * @serial
   */
  Thread _exitThread;
  
  /**
   * A boolean variable that is set to true if the application
   * is to be terminated; false otherwise
   *
   * @serial
   */
  protected boolean _terminateApp;

  /**
   * Sets the timeout length.
   * 
   * @param seconds the timeout period in milliseconds
   */
  public void setTimeoutLength(int milliseconds) {
    _servletTimeout = milliseconds;
  }
  
  /**
   * Starts timer that tries to sleep for the servlet timeout 
   * period. If the thread is not interrupted, then a terminate
   * action takes place. If the thread is interrupted before
   * the servelet timeout period expires, then the application
   * is not terminated. 
   */
  protected void startExitTimer() {

    // Wait until servlet times out, or until we are interrupted and
    // the application has been scheduled to terminate...
    while (!_terminateApp) {
      try {
        Thread.sleep(_servletTimeout);
        _terminateApp = true;
      } catch (Exception e) {
        // Do nothing--the exit thread has been interrupted.
      }
    }

    // Schedule the application to exit after EXIT_TIMEOUT_LENGHT milliseconds...
    try {
      Thread.sleep(EXIT_TIMEOUT_LENGTH);
    } catch (Exception e) {
      // Do nothing--for some unknown reason, the exit thread has been interrupted.
      // This should not happen, but if it does...
      System.out.println("Warning: The exit timer has been interrupted");
    } finally {
      System.err.println("Goodbye from server");
      System.exit(0);
    }
  }
  
  /**
   * This method is executed when the the execution of this thread 
   * starts.
   */
  public void run() {
    startExitTimer();
  }

  /**
   * Constructs a new message builder object with a default 
   * servlet timeout period of 10000 seconds.
   */
  public XmlMessageBuilder() {
    this(DEFAULT_TIMEOUT_INTERVAL);
  }

  /**
   * Constructs a new message builder object.
   * 
   * @param servletTimeout the servlet timeout period
   */
  public XmlMessageBuilder(int servletTimeout) {
  
    setTimeoutLength(servletTimeout);
    
    _terminateApp = false;
    buildBase();

    _exitThread = new Thread(this);
    _exitThread.start();
  }

  /**
   * Builds the XML for the "Message" and the "Reply" nodes.
   */
  protected void buildBase() {
    _message = new XML("Message");
    _reply = new XML("Reply");
    
    _message.addElement(_reply);
  }

  /**
   * Sends a return value from the CFE algorithms.  The return value is not sent
   * until the <code>flush()</code> method is called.
   *
   * @param windowID An index of the window ID within an application.  This
   *        index starts at '1'.
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param value The value for the action.  This field is ignored for some 
   *        action types.
   */
  public void putValue(int windowID, String action, String keyword, String value) {

    XML xmlAction = new XML(action);
    if (!"TerminateApp".equalsIgnoreCase(action)) {
      _terminateApp = false;
    } else {
      //System.err.println("terminateApp action recognized " + action);
      _terminateApp = true;      
    }

    xmlAction.addAttribute(WINDOW_ID, String.valueOf(windowID));
    if (keyword != null) {
      xmlAction.addAttribute(KEYWORD, keyword.toUpperCase());
    }
    if (value != null) {
      xmlAction.addAttribute(VALUE, value);
    }

    _reply.addElement(xmlAction);
    
  }

  /**
   * Sends an array of return values from the CFE algorithms.  The return
   * value is not sent until the <code>flush()</code> method is called.
   *
   * @param processID An index of the process ID within a single job.  This
   *        index starts at '1'.
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param array An array of values for the action.  This field is ignored
   *        for some action types.
   * @param start The start index within the array of values for the action.
   *        This index starts at '1'.
   * @param start The end index within the array of values for the action.
   *        This index starts at '1'.
   */
  public void putArray(int windowID, String action, String keyword, String[] array, int start, int end) {

    XML xmlAction = new XML(action);
    xmlAction.addAttribute(WINDOW_ID, String.valueOf(windowID));
    if (keyword != null) {
      xmlAction.addAttribute(KEYWORD, keyword.toUpperCase());
    }
    if ((array != null) &&
        (array.length != 0)) {
      xmlAction.addAttribute(ELEMENTS,    StringArray.output(array));            
    }
    
    if (start > 0) {        
      xmlAction.addAttribute(START_ELEMENT,  String.valueOf(start));
    }
    
    if (end > 0) {
      xmlAction.addAttribute(END_ELEMENT,    String.valueOf(end));
    }

    _reply.addElement(xmlAction);
  }

  /**
   * Retrieves the reply message from the message builder.  This is called
   * after the CFE method has been called.  The results are returned to the
   * the middle tier.
   *
   * @return The message that has been built using <code>putValue</code> and
   *         <code>putArray</code>. 
   */
  public String getMessage() {

    String retValue = _message.toString();
    
    // Interrupt the exit thread.  If the _terminateApp flag has been
    // set to true, the application will be scheduled to exit.  If not,
    // the application will reset its servlet timeout timer.
    _exitThread.interrupt();

    buildBase();

    //System.err.println("***************** ---> " + retValue);
    
    return retValue;
  }
}