// KeywordNotFoundException.java

package com.conoco.cfe.client.gui.controller;

/**
 * A class to define the exception that is thrown when a particular
 * keyword is not found by the application. For this application, 
 * all the components in the client have a keyword associated with 
 * them. 
 */
public class KeywordNotFoundException  extends Exception {
  /**
   * Declares a variable to store the keyword
   * 
   * @serial
   */
  protected String _keyword;
  
  /**
   * Constructs a a new exception
   */  
  public KeywordNotFoundException() {
    super();
  }    
  
  /**
   * Constructs a new exception object with the specified message.
   * 
   * @param s the message that describes the exception
   */  
  public KeywordNotFoundException(String s) {
    super(s);
  }    
  
  /**
   * Sets the keyword that the application is unable to find.
   * 
   * @param keyword the keyword
   */
  public void setKeyword(String keyword) { 
    _keyword = keyword;  
  }

  /**
   * Returns the keyword that the application is unable to find.
   * 
   * @return keyword the keyword
   */  
  public String getKeyword() {
    return _keyword;
  }
}