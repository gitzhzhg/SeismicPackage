// LoginServlet.java

package com.conoco.cfe.servlet;

import java.io.IOException;
import java.io.PrintWriter;

import java.rmi.ServerException;

import javax.servlet.ServletException;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.SingleThreadModel;


/**
 * Servlet for handling the login validation.
 */
public class LoginServlet extends HttpServlet
  implements SingleThreadModel {
    
  /**
   * Default Backend timeout interval
   *
   */
  private final static int DEFAULT_TIMEOUT_INTERVAL = 61 * 60 * 1000;
  
  /**
   * Handle the HTTP "POST" request.
   */  
    public void doPost(HttpServletRequest request,
           HttpServletResponse response)
  throws IOException, ServletException {

    String user = null;
    String password = null;
    String serverName = null;
    String scriptName = null;
    String result = null;
    
      user     = (String)request.getParameter(ConocoInformation.POST_VARIABLE_USER);
      password  = (String)request.getParameter(ConocoInformation.POST_VARIABLE_PASSWORD);
    serverName  = (String)request.getParameter(ConocoInformation.POST_VARIABLE_SERVER);
      scriptName  = (String)request.getParameter(ConocoInformation.POST_VARIABLE_SCRIPT);

      if (user != null && password != null && serverName != null && scriptName != null) {
      HttpSession session = request.getSession(true);
      String sessionId    = session.getId();
      int sessionTimeout  = DEFAULT_TIMEOUT_INTERVAL;
    
    
      try {
        ConocoRMIHandler.establishConnection(serverName, 
                          user,
                          password,
                          scriptName,
                          sessionId,
                          sessionTimeout);
      
        result = "success";
  
      }
      catch (IOException ioe) {
        result = "IOException in establishing connection to server" + ioe.toString();
      }
      catch (Exception e) {
        result = e.toString();
      }
      }
      else {
        if (user == null) {
          result = "User was null";
        }
        else
        if (password == null) {
          result = "Password was null";
        }
        else
        if (serverName == null) {
          result = "Server name was null";
        }
        else
        if (scriptName == null) {
          result = "Script name was null";
        }
      }
        
    response.setContentType("text/plain");
    PrintWriter writer = response.getWriter();
    
    writer.println(result);
    
    writer.flush();
    writer.close();

  }

  /**
   * Handle the HTTP "GET" request. Default to "POST".
   */
    public void doGet(HttpServletRequest request,
          HttpServletResponse response)
  throws IOException, ServletException {
  
    doPost(request, response);
    }
  
      
}