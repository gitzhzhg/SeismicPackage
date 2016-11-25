// ConocoStartup.java

package com.conoco.cfe.servlet;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.ServletConfig;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.SingleThreadModel;

/**
 * The startup servlet that gets the initial parameter information.
 */
public class ConocoStartup extends HttpServlet 
  implements SingleThreadModel {
  
  /**
   * Initializes this servlet.
   * 
   * @param config the servlet configuration object
   */
  public void init(ServletConfig config) {
    ConocoInformation.load(config.getInitParameter("ParameterDir"));
  }

  /**
   * Receives an HTTP GET request and processes this request.
   * 
   * @param request object that encapsulates the client request
   * @param response object that encapsulates the the response to be sent to the client
   * @throws java.io.IOException  if an input or output error is detected when the servlet handles the GET request
   */
  public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
  }
}