///
/// JNICfeApi.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  5. 10-08-2003 SMCook   Added TimeStamp calls.
///  4. 09-27-2002 SMCook   Moved protective setArray code elsewhere to fix bug
///                          wherein deleted array text would later "reappear"
///                          (the back end was not being notified of certain
///                          array cell changes).
///                         Made most lines of code adhere to an 80-char limit.
///  3. 08-13-2002 SMCook   Added protective code to setArray to prevent it
///                          from sending nulls.
///

package com.conoco.cfe.server.messaging;

import com.conoco.shared.TimeStamp;

import com.conoco.cfe.client.application.Console;

/**
 * This class provides access to native CFE API calls via JNI (Java Native
 * Interface).  The native CFE calls can, in turn, create reply messages to be
 * sent to the client.
 * Reply messages are built using
 *
 *   <code>com.conoco.cfe.server.messaging.MessageBuilder</code>.
 *
 * @see com.conoco.cfe.server.messaging.MessageBuilder
 */
public class JNICfeApi implements CfeApi {
  
  /**
   * Load the native library everytime the class is loaded.
   * initIds calls a native method to initialize some C++ code
   */
  static {
    String libraryName = "opencfe";

    try {
      System.loadLibrary(libraryName);
    }
    catch (UnsatisfiedLinkError e) {
      System.out.println("Unable to load dynamic library " + libraryName);
      System.exit(0);
    }
    
    initIds();
  }
  
  protected MessageBuilder _messageBuilder = null;
  
  private static native void initIds();

  private native void set_value(
    int windowID, String action, String keyword, String value)
    throws MessagingException;

  private native void set_array(
    int windowID, String action, String keyword, String[] array,
    int start, int end) throws MessagingException;

  /**
   * Constructs the JNI Cfe API access object
   */
  public JNICfeApi() {
  }

  /**
   * Retrieves message builder used by CFE API to construct reply messages.
   *
   * @return The message builder used by the CFE API to construct messages.
   */
  public MessageBuilder getMessageBuilder() {
    return _messageBuilder;
  }

  /**
   * Sets message builder instance used by CFE API to construct reply messages.
     * 
   * @param messageBuilder the instance that will handle reply messages
   */
  public void setMessageBuilder(MessageBuilder messageBuilder) {
    _messageBuilder = messageBuilder;
  }

  /**
   * Sends a value to the CFE algorithms.
   *
   * @param windowID An index of the window ID within an application.  This
   *        index starts at '1' for process based windows.
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param value The value for the action.  This field is ignored for some 
   *        action types.
   */
  public void setValue(
    int windowID, String action, String keyword, String value)
    throws MessagingException {        

    Console.logMessage(TimeStamp.getTimeStamp() + "\n");
    set_value(windowID, action, keyword, value);
  }

  /**
   * Sends an array of values to the CFE algorithms.
   *
   * @param windowID An index of the window ID within an application.  This
   *        index starts at '1' for process based windows.
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param array An array of values for the action.  This field is ignored
   *        for some action types.
   * @param start The start index within the array of values for the action.
   *        This index starts at '1'.
   * @param end The end index within the array of values for the action.
   *        This index starts at '1'.
   */
  public void setArray(
    int windowID, String action, String keyword, String[] array,
    int start, int end) throws MessagingException {

    //SMCook - at one point was returning here if array was null.
    //         "reappearing text" bug resulted - null array must be sent.

    Console.logMessage(TimeStamp.getTimeStamp() + "\n");
    set_array(windowID, action, keyword, array, start, end);
  }
  
  /**
   * Relays the putValue call to the messageBuilder
   *
   * @param windowID An index of the window ID within an application.  This
   *        index starts at '1' for process based windows.
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param value The value for the action.  This field is ignored for some 
   *        action types.
   */
  protected void putValue(
    int windowID, String action, String keyword, String value)
    throws MessagingException {

    if (_messageBuilder != null) {
      _messageBuilder.putValue(windowID, action, keyword, value);
    }
  }
  
  /**
   * Relays the putArray call to the messageBuilder
   *
   * @param windowID An index of the window ID within an application.  This
   *        index starts at '1' for process based windows.
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param array An array of values for the action.  This field is ignored
   *        for some action types.
   * @param start The start index within the array of values for the action.
   *        This index starts at '1'.
   * @param end The end index within the array of values for the action.
   *        This index starts at '1'.
   */
  protected void putArray(
    int windowID, String action, String keyword,
    String[] array, int start, int end)
    throws MessagingException {

    if (_messageBuilder != null) {
      _messageBuilder.putArray(windowID, action, keyword, array, start, end);
    }
  }
}
