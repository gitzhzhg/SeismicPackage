// MessagingException.java

package com.conoco.cfe.server.messaging;

/**
 * The MessagingException class is used to terminate message processing when an error
 * has occurred. It can be raised both in Java and in native code (if required). 
 * It is expected that the MessageTransport implementation class will catch the
 * MessagingException and report the error back to the caller.
 */
public class MessagingException extends Exception {
  
  /** 
   * The description of the error that occurred
   *
   * @serial
   */
  protected String _errorMessage;
  
  /**
   * Construct the Exception including the error message
   *
   * @param errorMessage a description of the error that has occurred
   */
  public MessagingException(String errorMessage) {
    _errorMessage = errorMessage;
  }
  
  /**
   * Overriding toString provides a shorthand for retrieving 
   * the error message string.
   * 
   * @return the error message
   */
  public String toString() {
    return _errorMessage;
  }
  
}