// UserValidator.java

package com.conoco.cfe.client.messaging;

/**
 * Interface for a login validator. The 
 * login validator communicates with
 * the web server for user name validation.
 */
public interface UserValidator {
  /**
   * Performs initialization.
   * 
   * @return   a boolean value that is true if 
   *       initialization is successful
   */
   public boolean init();
   
  /**
   * Sets the user name.
   * 
   * @param user the name of the user
   */
  public void setUser(String user);
  
  /**
   * Sets the password.
   * 
   * @param password the password
   */
  public void setPassword(String password);
  
  /**
   * Sends the login information to the server 
   * for the currently set user and password.
   * 
   * @return the server response as a <code>java.lang.String</code>
   */
  public String login();  
}