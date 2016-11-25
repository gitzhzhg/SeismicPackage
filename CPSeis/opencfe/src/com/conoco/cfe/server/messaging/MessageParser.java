// MessageParser.java

package com.conoco.cfe.server.messaging;

/**
 * This is the interface used to parse messages into CFE API calls.
 *
 * @see com.conoco.cfe.server.messaging.CfeApi
 */
public interface MessageParser extends MessagingConstants {

  /**
   * Parses a message.  After the message has been parsed, this method calls
   * the appropriate methods on the CFE API to send the message to the
   * appropriate CFE algorithm.
   * 
   * @param message the message to be parsed
   * @throws   com.conoco.cfe.server.messaging.MessagingException if an error
   *       occurs while parsing the message
   */
  public void parseMessage(String message)
    throws MessagingException;
}