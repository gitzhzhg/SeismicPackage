///
/// ConocoRMIHandler.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  4.
///  3. 09-24-2001 SMCook   Found and fixed the probable cause of the
///                          long-standing CFE "startup hang".  There was a
///                          while(); statement in the sleep logic that
///                          occasionally could loop indefinitely.
///                         Also, increased MaxConnectAttempts from 10 to 30
///                          and made cosmetic/readability changes.
///

package com.conoco.cfe.servlet;

import com.conoco.cfe.interfaces.MessageTransport;

import java.net.MalformedURLException;

import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.ConnectException;
import java.rmi.RemoteException;

import java.io.File;
import java.io.IOException;

/**
 * A handler to establish an RMI connection with the backend server. 
 * The message passer servlet, <code>com.conoco.cfe.servlet.MessagePasser</code>
 * uses this handler to establish the connection with the backend server and
 * consequently to retrieve a reference to the backend message transport object.
 */
public class ConocoRMIHandler {

  /** 
   * How many times to try to reconnect to rmiregistry
   * 
   * @serial
   */
  private final static int MaxConnectAttempts = 30;

  /**
     * Establish a connection to the seismic process server. This method
     * is only called from the login servlet.
     *
     * @param serverName The IP name of the seismic processing server.
     * @param commandName The name of the EZ-ED command to be run on the seismic processing server.
     * @param sessionId The servlet session ID, used to establish a unique connection
     * to the seismic processing server.
     */
    static public MessageTransport establishConnection(String serverName, 
                            String userName,
                            String password,
                            String scriptName, 
                            String sessionId,
                            int timeOutLength)
    throws IOException, GenericErrorException {


    MessageTransport messageTransport = null;
    String launchCommand = ConocoInformation.SCRIPT_BASE_DIR + "/" + 
                 scriptName + " " +
                 serverName + " " + 
                 userName + " " + 
                 sessionId + " " + 
                 timeOutLength + "; exit";

    // RMI Registry Name
    String rmiName = "//" + serverName + "/" + sessionId;

    // Create the process and authenticate the login
    int result = ProcessCommunicator.runCommand(serverName, userName, password, launchCommand);

    // SUCCESS !. Assuming all went well with the launch of the
    //  script, the rmi object should appear in the registry
    //  this may take time so we loop for MaxConnectAttempts
    //  until we find it.

    if (result == ProcessCommunicator.SUCCESS) {
      boolean notConnected = true;
      int count = 0;

      while ((notConnected) && (count < MaxConnectAttempts)) {
        try {
          messageTransport = (MessageTransport)Naming.lookup(rmiName);
          notConnected = false;
        } 
        catch (NotBoundException nbe2) {
          sleepForASecond();
          count++;
        }
 
        if (count >= MaxConnectAttempts) {
          String errorMessage = "Error connecting to : " + 
                    rmiName + "\n" +
                    "Tried to connect " + count + " times.  Aborting\n";
          throw(new GenericErrorException(errorMessage + "\n" + launchCommand, true));
        }
      }
    }
    else if (result == ProcessCommunicator.LOGIN_FAILED) {
      throw new GenericErrorException("Login failed", true);
    }
    else if (result == ProcessCommunicator.TRANSPORT_ERROR) {
      throw new GenericErrorException("Problem with telnet connectivity. Login failed", true);
    }
    else if (result == ProcessCommunicator.LOGIN_NOT_FOUND) {
      throw new GenericErrorException("Login prompt not found", true);
    }
    else if (result == ProcessCommunicator.PASSWORD_NOT_FOUND) {
      throw new GenericErrorException("Password prompt not found", true);
    }
    else if (result == ProcessCommunicator.PROCESS_EXITED) {
        throw new GenericErrorException("The backend server died unexpectedly ", true);
    }
    else {
        throw new GenericErrorException("Unhandled result.  Please report.", true);
    }

    return messageTransport;
  }


    /**
     * Login has established a connection to the seismic process server. Therefore
     * subsequent connections only need to do a lookup to the rmiregistry.
     *
     * @param serverName The IP name of the seismic processing server.
     * @param commandName The name of the EZ-ED command to be run on the seismic processing server.
     * @param sessionId The servlet session ID, used to establish a unique connection
     * to the seismic processing server.
     */
    static public MessageTransport getConnection(String serverName, 
                           String sessionId)
    throws IOException, GenericErrorException {
  
    MessageTransport messageTransport = null;
    
    // Lookup the RMI name.  If we cannot find the appropriate connection, assume the
    // process has died--restart.

    try {
        messageTransport = (MessageTransport)Naming.lookup("//" + serverName + "/" + sessionId);
    } 
    catch (NotBoundException nbe) {
      throw new GenericErrorException("The backend server has died unexpectedly ", true);
    }
  
    return messageTransport;
    }
    
    /**
     * On some systems (ie solaris) the sleep call will get interrupted
     * so we just record the time and keep calling sleep until 
     * at least a second has elapsed.
     */
    private static void sleepForASecond() {
      long startTime = System.currentTimeMillis();
      long currentTime = 0;
      while(currentTime - startTime < 1000) {
        try {
          Thread.sleep(1010);
        } catch (InterruptedException ie) {
          // Do nothing
        }
        currentTime = System.currentTimeMillis();
      }
    }
}
