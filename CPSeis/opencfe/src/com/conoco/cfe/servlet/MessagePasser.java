// MessagePasser.java

package com.conoco.cfe.servlet;

import com.conoco.cfe.interfaces.MessageTransport;

import java.io.IOException;
import java.io.PrintWriter;

import java.rmi.RemoteException;
import java.rmi.ServerException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.SingleThreadModel;

import com.conoco.xml.XML;
import com.conoco.xml.TextNode;

/**
 * This class passes messages from a client to a seismic processing server application,
 * runs a seismic process, then returns the resulting message back to the client.
 */
public class MessagePasser extends HttpServlet {  
        
    /**
     * Transmits a message from the client to the server, then transmits the response from
     * the server back to the client.
     *
     * @param serverName The IP address of the seismic processing server.
     * @param request The HTTP request that initiated this program.
     * @param message The message that will be passed to the server process.
     * @throws com.conoco.cfe.servlet.GenericErrorException if there is an error 
     * in the back end server processing or if there is tranport failure
     */
    protected void transmitMessage(PrintWriter pWriter, String serverName, String sessionId, String message) 
    throws GenericErrorException {

    MessageTransport messageTransport = null;
    try {
      messageTransport = ConocoRMIHandler.getConnection(serverName, sessionId);

    }
    catch (IOException ioe) {
      sendErrorAndExit("Failed to establish connection to server\n" + ioe);
    }
    catch (Exception e) {
      sendErrorAndExit("Failed to establish connection to server\n" + e);
    }
    
    if (messageTransport != null) {
      try {
           String returnMsg = messageTransport.sendMessage(message);
           sendReply(pWriter, returnMsg);
      }
      catch (ServerException se) {
        sendErrorAndExit("Error from backend server\n" + se.getMessage());
      }
      catch (RemoteException re) {
        sendErrorAndExit("Error from backend server\n" + re.getMessage());
      }
      catch (Exception e) {
        sendErrorAndExit("Transport Failure--\n" + e + "--"+e.getMessage());
      }
        
    } else {
         sendErrorAndExit("Could not establish connection to server");
    } 
    }
    
    /**
     * Sends a message to the client over the HTTP protocol.
     *
     * @param message The XML message that will be sent to the client.
     */
    public void sendReply(PrintWriter output, String message) {
      
      // PENDING(gww) Better check that the remote server sends a valid document
      // with header otherwise this is a pile.
    output.println(message);
    
    }
    
    /**
     * Sends an error message to the client.
     *
     * @param errorMsg the error message that is to be sent to the client
     */
    public void sendError(PrintWriter pWriter, String sessionId, String errorMsg) {
      XML message = new XML("Message");
      XML reply   = new XML("Reply");  
    XML error = new XML("Error");
    
    error.addAttribute("elements", errorMsg);
    reply.addElement(error);
    message.addElement(reply);
    
    // If there is an error before the session has started then
    // the sessionId is null
    if (sessionId != null) {
      error.addAttribute("windowId", sessionId);
    }
    else {
      error.addAttribute("windowId", "None");
    }
        
    sendReply(pWriter, buildHeader("Message") + message.toString());
    }
  
    /**
     * Sends an error message to the client and exit.  This is used whenever there is a problem
     * before a connection has been established with the server, or if a problem occurs
     * with the connection to the server.
     *
     * @param errorMsg The error message that will be sent.  This should not have
     * XML markups for the Error tag.
     * @throws com.conoco.cfe.servlet.GenericErrorException
     */
    protected void sendErrorAndExit(String errorMsg) throws GenericErrorException {
    throw(new GenericErrorException(errorMsg, true));
    }
    
    /**
     * Build the XML header for the return message.
     *
     * @param documentType A character string repersenting the XML element that acts
     * as the root of the XML message.
     * @return The first two lines in an XML message (XML version #, document type,
     * and DTD location).
     */
    protected String buildHeader(String documentType) {
    return ("<?xml version=\"1.0\" ?>" + 
      "\n<!DOCTYPE " + 
      documentType + 
      " SYSTEM \"" + 
      ConocoInformation.MESSAGE_DTD + 
      "\">\n");
    }
    
  
    /**
     * This is the method that is called when the servlet is run.  This method
     * passes the message sent by the client to the seismic processing server.
     * When the appropriate method on the server has been run, it relays the 
     * resulting message from the server to the client.
     *
     * @param request The object that manages the servlet request.
     * @param response The object that manages the response to the servlet request.
     * @throws java.io.IOException if there is an error in reading the request information
     * @throws javax.servlet.ServletException if the request for POST cannot be handled
     */
    public void doPost(HttpServletRequest request,
           HttpServletResponse response) throws IOException, ServletException {
  
    String sessionId = null;
    PrintWriter pWriter = null;
    
    try {
    
        response.setContentType("text/xml");
        pWriter = response.getWriter();
      
        HttpSession session = request.getSession(false);
      sessionId = session.getId();
        
        String serverName = (String)request.getParameter(ConocoInformation.POST_VARIABLE_SERVER);
        String clientMessage = (String)request.getParameter(ConocoInformation.POST_VARIABLE_MESSAGE);
      
        // Get the name of the seismic processing server from the client
        if (serverName == null) {
        sendErrorAndExit("No server is given");
        }
      
        // Get the message that will be transmitted to the server
        if (clientMessage == null) {
        sendErrorAndExit(ConocoInformation.POST_VARIABLE_MESSAGE + ": No message is given");
        }
      
        // Transmit the message to the server
        transmitMessage(pWriter, serverName, sessionId, clientMessage);
      
    } catch (GenericErrorException e) {
      sendError(pWriter, sessionId, e.toString());
        // Somewhere in this servlet, an error message has been sent; exit immediately
        return;
    }
    }

    /**
     * In case someone calls us using the GET method, re-route the request to the POST
     * handler .
     *
     * @param request The HTTP request object.
     * @param response The HTTP response to the request.
     * @throws java.io.IOException if there is an error in reading the request information
     * @throws javax.servlet.ServletException if the request for GET cannot be handled
     */
    public void doGet(HttpServletRequest request,
          HttpServletResponse response)
  throws IOException, ServletException {
  
    doPost(request, response);
    }
    
    
}