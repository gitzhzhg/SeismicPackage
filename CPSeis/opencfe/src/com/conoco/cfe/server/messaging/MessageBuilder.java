package com.conoco.cfe.server.messaging;

/**
 * This class is used by the CFE API to build reply messages to be sent to the
 * client.
 */
public interface MessageBuilder extends MessagingConstants {

  /**
   * Sends a return value from the CFE algorithms.  The return value is not sent
   * until the <code>flush()</code> method is called.
   *
   * @param windowID An index of the window ID within an application.  This
   *        index starts at '1' for process based windows.
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param value The value for the action.  This field is ignored for some 
   *        action types.
   */
  public void putValue(int windowID, String action, String keyword, String value);

  /**
   * Sends an array of return values from the CFE algorithms.  The return
   * value is not sent until the <code>flush()</code> method is called.
   *
   * @param windowID An index of the process ID within a single job.  This
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
  public void putArray(int windowID, String action, String keyword, String[] array, int start, int end);

  /**
   * Retrieves the reply message from the message builder.  This is called
   * after the CFE method has been called.  The results are returned to the
   * the middle tier.
   *
   * @return The message that has been built using <code>putValue</code> and
   *         <code>putArray</code>. 
   */
  public String getMessage();
}