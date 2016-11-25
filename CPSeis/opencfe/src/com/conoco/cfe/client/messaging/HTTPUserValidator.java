// HTTPUserValidator.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;

import java.io.PrintWriter;
import java.io.OutputStreamWriter;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.net.MalformedURLException;

/**
 * Sets up the communication between the client and the
 * server for the purpose of user login validation. The
 * URL of the servlet that handles the login request 
 * is specified in the preferences file. The login 
 * information is passed to the server on HTTP.
 * 
 * @see preferences.dtd
 */
public class HTTPUserValidator implements UserValidator {
  /**
   * Variable for the user name
   * 
   * @serial
   */
  protected String _user;
  
  /**
   * Variable for the password
   * 
   * @serial
   */
  protected String _password;
  
  /**
   * Variable for the login servlet URL
   * 
   * @serial
   */
  protected URL _url;
    
  /**
   * Constructs a new validator object.
   */  
  public HTTPUserValidator() {
  }
  
  /**
   * Performs initialization.
   * 
   * @return   a boolean value that is true if 
   *       initialization is successful
   */
  public boolean init() {
    try {
      _url = new URL(Preferences.getLoginServletName());
    } 
    catch (MalformedURLException en) {
    }
    return !(_url == null);
  }  
  
  /**
   * Sets the user name.
   * 
   * @param user the name of the user
   */
  public void setUser(String user) {
     _user = user;
  }
   
  /**
   * Sets the password.
   * 
   * @param password the password
   */
  public void setPassword(String pass) {
    _password = pass;
  }
  
  /**
   * Sends the login information to the server 
   * for the currently set user and password.
   * 
   * @return the server response as a <code>java.lang.String</code>
   */
  public String login() {
    URLConnection urlConn = null;
    try {
      if ( _url != null ) {
        urlConn = _url.openConnection();
      } 
      else {
        String[] messages = { "HTTPUserValidator: Login Servlet URL is null" };
        ErrorInfoWarningDialog.showErrorDialog(messages);
        System.out.println(messages[0]);
        System.exit(1);
      }
      urlConn.setDoInput(true);
      urlConn.setDoOutput(true);
      urlConn.setUseCaches(false);
      urlConn.setRequestProperty("Content-type", "application/x-www-form-urlencoded");
    
      PrintWriter httpOutput = new PrintWriter( new OutputStreamWriter(urlConn.getOutputStream()));
      
      httpOutput.print("user="+ URLEncoder.encode(_user));
      httpOutput.print("&password=" + URLEncoder.encode(_password));
      httpOutput.print("&serverName=" + Preferences.getServerName());
      httpOutput.print("&scriptName=" + Preferences.getScriptName());
    
      httpOutput.flush();
      httpOutput.close();
    } 
    catch ( Exception e) {
      String[] messages = { "HTTPUserValidator: Error Sending Login Information", e.toString() };
      ErrorInfoWarningDialog.showErrorDialog(messages);
      System.out.println(messages[0] + e);
      System.exit(1);
    }
    String field = null;
    String value = "initial";
    for (int i = 0; ((!"Set-Cookie".equals(field)) && (value != null)); i++) {
      field = urlConn.getHeaderFieldKey(i);
      value = urlConn.getHeaderField(i);
    }
    if ("Set-Cookie".equals(field)) {
      int separatorLoc = value.indexOf("=");
      ClientConstants.COOKIE_NAME = value.substring(0, separatorLoc);
      ClientConstants.COOKIE_VALUE = value.substring((separatorLoc + 1), value.indexOf(";"));
      System.out.println("CookieName: " + ClientConstants.COOKIE_NAME + " CookieValue: " + ClientConstants.COOKIE_VALUE);
    }    
    StringBuffer result = new StringBuffer();
    try {
      BufferedReader in = new BufferedReader(new InputStreamReader(urlConn.getInputStream()));
      String line;
      while(null != ((line = in.readLine()))) {
        //System.out.println(line);
         result.append(line);
      }
      in.close();            
    } 
    catch (Exception e) {
      String[] messages = { "HTTPUserValidator: Login Failed", e.toString() };
      ErrorInfoWarningDialog.showErrorDialog(messages);
    }          
    return result.toString();
  }
}
