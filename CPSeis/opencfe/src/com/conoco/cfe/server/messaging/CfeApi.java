package com.conoco.cfe.server.messaging;

/**
 * This interface provides access to native CFE API calls.  The native CFE
 * calls can, in turn, create reply messages to be sent to the client.  Reply
 * messages are built using the <code>MessageBuilder</code>.
 *
 * @see MessageBuilder
 */
public interface CfeApi extends MessagingConstants {

  /**
   * Retrieves the message builder used by the CFE API to construct reply messages.
   *
   * @return The message builder used by the CFE API to construct messages.
   */
  public MessageBuilder getMessageBuilder();

  /**
   * Sets the message builder instance used by the CFE API to construct reply messages.
         * 
   * @param messageBuilder the instance that will handle reply messages
   */
  public void setMessageBuilder(MessageBuilder messageBuilder);

  /**
   * Sends a value to the CFE algorithms.
   *
   * @param processID An index of the process ID within a single job.  This
   *        index starts at '1'.
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param value The value for the action.  This field is ignored for some 
   *        action types.
   */
  public void setValue(int processID, String action, String keyword, String value) throws MessagingException;

  /**
   * Sends an array of value to the CFE algorithms.
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
  public void setArray(int processID, String action, String keyword, String[] array, int start, int end) throws MessagingException;

}