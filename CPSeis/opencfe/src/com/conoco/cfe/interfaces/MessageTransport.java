package com.conoco.cfe.interfaces;

import java.rmi.Remote;

/**
 * The transport layer for transmitting messages to and from the server.
 */
public interface MessageTransport extends Remote {
  
  /**
   * Sends a message from the client to the server.  When the server has
   * finished processing the message, a reply message is sent back to the
   * server.
   *
   * @param clientMessage The message that will be sent from the client to
   *        the server.
   * @return The reply message sent from the server to the client.
   */
  public String sendMessage(String clientMessage)
    throws Exception;
}