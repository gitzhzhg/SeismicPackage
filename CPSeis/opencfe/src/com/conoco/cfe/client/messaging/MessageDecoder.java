// MessageDecoder.java

package com.conoco.cfe.client.messaging;

import java.util.Hashtable;

/**
 * A class that decodes the XML document describing the server reply.
 * The decoder is called by the client's state machine to decode
 * the message coming from the server. 
 */
public interface MessageDecoder {
  /**
   * Decodes a given string that is an XML document 
   * describing the server's reply.
   * 
   * @param message the XML document describing server's reply
   */
  public void decode(String message);
  
  /**
   * Sets the action handlers that will handle the server
   * reply messages.
   * 
   * @param h the action handlers 
   */ 
  public void setReplyActionHandlers(Hashtable h);
}