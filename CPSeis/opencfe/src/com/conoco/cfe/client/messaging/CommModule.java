// CommModule.java

package com.conoco.cfe.client.messaging;

/**
 * A communications module that acts as a middleman between
 * a client's state machine and a server. 
 */
public interface CommModule {
  /**
   * Sends the message, specified as a string, to the server and
   * returns back the server reponse.
   * 
   * @param message the client's request 
   * @return the server's response 
   */
  public String sendMessage(String message);
}