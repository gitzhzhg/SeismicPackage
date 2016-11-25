// GenericErrorException.java

package com.conoco.cfe.servlet;

/**
 * This Exception class is used to pass an error message back 
 * to the servlet handler method, normally doPost. It is used by the servlet in lieu of
 * <code>System.exit()</code> in order to immediately exit the servlet.
 */
public class GenericErrorException extends Exception {
  
  /**
   * Variable for the error message
   * 
   * @serial
   */
  protected String   _errorMessage;
  
  /**
   * Variable for a boolean flag that is set to true if the 
   * exit action is to be executed
   * 
   * @serial
   */
  protected boolean  _exitFlag;
  
  /**
   * Constructs a new exception.
   * 
   * @param errorMessage the error message 
   * @param exitFlag the boolean flag for the exit action
   */
  public GenericErrorException(String errorMessage, boolean exitFlag) {
       super();
       
       _errorMessage = errorMessage;
       _exitFlag     = exitFlag;
  }
  
  /**
   * Constructs a new exception.
   * 
   * @param errorMessage the error message 
   */
  public GenericErrorException(String errorMessage) {
    this(errorMessage, false);
  }
  
  /**
   * Returns a boolean variable to indicate whether exit action is 
   * to be executed or not.
   * 
   * @return the boolean that is used to determine the execution of exit action
   */
  public boolean isExit() {
    return _exitFlag;
  }
  
  /**
   * Returns a string representation of this exception.
   *
   * @return the error message string
   */
  public String toString() {
    return _errorMessage;
  }
}