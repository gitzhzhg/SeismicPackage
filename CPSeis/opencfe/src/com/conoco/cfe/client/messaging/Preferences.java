// Preferences.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.ClientConstants;

/**
 * A class to store the application preferences like server name, 
 * the servlet name and others. The preferences file is specified
 * as an argument to the client at application startup. The 
 * preferences file is read and parsed by 
 * <code>com.conoco.cfe.messaging.PreferencesReader</code>.
 * 
 * @see com.conoco.cfe.messaging.PreferencesReader
 */
public abstract class Preferences {
  
  /**
   * Declares a variable to store the server name
   * 
   * @serial
   */
  protected static String _serverName = ClientConstants.DEFAULT_SERVER_NAME;
  
  /**
   * Declares a variable to store the login servlet name
   * 
   * @serial
   */
  protected static String _loginServletName = ClientConstants.DEFAULT_LOGIN_SERVLET_NAME;
  
  /**
   * Declares a variable to store the RMI script name
   * 
   * @serial
   */
  protected static String _scriptName = ClientConstants.DEFAULT_SCRIPT_NAME;
  
  /**
   * Declares a variable to store the servlet name
   * 
   * @serial
   */
  protected static String _servletName = ClientConstants.DEFAULT_SERVLET_NAME;
  
  /**
   * Declares a variable to store the XML document path
   * 
   * @serial
   */
  protected static String _xmlDocPath = ClientConstants.DEFAULT_XML_DOC_PATH;

  /**
   * Declares a variable to store the path for the backend server's libraries
   *
   * @serial
   */
  protected static String _backEndLibPath = ClientConstants.DEFAULT_BACKEND_LIB_PATH;
  
  /**
   * Variable for the help mode
   *
   * @serial
   */
  protected static String _helpMode = "mouse";
      
  /**
   * Sets the server name 
   * 
   * @param name the name of the server
   */
  public static void setServerName(String name) {
    _serverName = name;  
  }  
  
  /**
   * Returns the server name 
   * 
   * @return name the name of the server
   */
  public static String getServerName() {
    return _serverName;  
  }  
  
  /**
   * Sets the login servlet name 
   * 
   * @param name the name of the servlet
   */
  public static void setLoginServletName(String name) {
    _loginServletName = name;  
  }  
  
  /**
   * Returns the login servlet name 
   * 
   * @return name the name of the login servlet
   */
  public static String getLoginServletName() {
    return _loginServletName;  
  }  
  
  /**
   * Sets the RMI script name 
   * 
   * @param name the name of the RMI script
   */
  public static void setScriptName(String name) {
    _scriptName = name;  
  }  
  
  /**
   * Returns the RMI script name 
   * 
   * @return name the name of the RMI script
   */
  public static String getScriptName() {
    return _scriptName;  
  }  
  
  /**
   * Sets the servlet name 
   * 
   * @param name the name of the servlet   
   */
  public static void setServletName(String name) {
    _servletName = name;  
  }  
  
  /**
   * Returns the servlet name 
   * 
   * @return name the name of the servlet
   */
  public static String getServletName() {
    return _servletName;  
  }  
  
  /**
   * Sets the XML document path
   * 
   * @param name the XMl document path   
   */
  public static void setXMLDocumentPath(String name) {
    _xmlDocPath = name;  
  }  
  
  /**
   * Returns the servlet name 
   * 
   * @return name the name of the servlet
   */
  public static String getXMLDocumentPath() {
    return _xmlDocPath;  
  }  

  /**
   * Sets the backend's library path.
   * 
   * @param path the desired path   
   */
  public static void setBackEndLibPath(String path) {
    _backEndLibPath = path;  
  }  
  
  /**
   * Returns the backend's library path. 
   * 
   * @return the backend's library path
   */
  public static String getBackEndLibPath() {
    return _backEndLibPath;  
  }
  
  /**
   * Sets the help mode.
   * 
   * @param mode the help mode
   */
  public static void setHelpMode(String mode) {
    _helpMode = mode;  
  }  
  
  /**
   * Gets the help mode.
   * 
   * @param path the help    
   */
  public static String getHelpMode() {
    return _helpMode;  
  }  
}