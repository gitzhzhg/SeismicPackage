// HTTPCommModule.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.Client;
import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;

import com.conoco.cfe.client.messaging.CommModule;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.BufferedReader;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.net.HttpURLConnection;

import java.util.Hashtable;

import javax.swing.JFrame;
import javax.swing.Timer;

/**
 * A communications module that acts as a middleman between
 * a client's state machine and a server which communicates
 * with the client using the HyperText Transfer Protocol (HTTP).
 * 
 */
public class HTTPCommModule implements CommModule {
  /**
   * Declares the variable to hold the server URL
   * 
   * @serial
   */
  protected URL _url;
  
  /**
   * Declares the variable to hold the cookie name
   * 
   * @serial
   */
  protected String _cookieName;
  
  /**
   * Declares the variable to hold the cookie value
   * 
   * @serial
   */
  protected String _cookieValue;
  
  /**
   * Inactivity timer
   *
   */
  protected Timer _timer;
    
  /**
   * Constructs a communications module with null URL.
   */
  public HTTPCommModule() {
  }
  
  /**
   * Constructs a communications module with the specified server URL. 
   *
   * @param url the URL of the server
   */
  public HTTPCommModule(URL url) {  
    this();
    setURL(url);
  }
  
  /**
   * Sets the URL of the server.
   * 
   * @param url the URL of the server
   */
  public void setURL(URL url) {
    _url = url;
  }
  
  /**
   * Returns the URL of the server.
   * 
   * @return the server URL
   */
  public URL getURL() {
    return _url;
  }
            
  /**
   * This actually sets up the communication with the server 
   * for the trnasmittal of the request. The string  
   * is the XML document that describes the request. 
   * 
   * @param document the XML document describing the request message
   * @return the reply of the server as a XML document string
   */
  public String sendMessage(String document) {
    URLConnection urlConn = null;
    try {
      stopTimer();
      if ( _url != null ) {
        urlConn = _url.openConnection();
      } 
      else {
        String[] messages = { "HTTPCommModule: Servlet URL is null" };
        ErrorInfoWarningDialog.showErrorDialog(messages);
        return null;
      }
      urlConn.setDoInput(true);
      urlConn.setDoOutput(true);
      urlConn.setUseCaches(false);
      urlConn.setRequestProperty("Content-type", "application/x-www-form-urlencoded");
      if ( ClientConstants.COOKIE_NAME  != null ) {
        urlConn.setRequestProperty("Cookie: ", ClientConstants.COOKIE_NAME + "=" +
          ClientConstants.COOKIE_VALUE);
      }
      ByteArrayOutputStream ba = new ByteArrayOutputStream();
      PrintWriter xmlWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(ba)));

      xmlWriter.print(document);
      xmlWriter.flush();
      
      PrintWriter httpOutput = new PrintWriter( new OutputStreamWriter(urlConn.getOutputStream()));
      httpOutput.print("serverName="+ Preferences.getServerName());
      httpOutput.print("&clientMessage=");      
      System.out.println("HTTPCommModule: Sending Message ---->");
      System.out.println(ba.toString());
      httpOutput.write(URLEncoder.encode(ba.toString()));
      httpOutput.flush();
      httpOutput.close();
      xmlWriter.close();
    } 
    catch ( Exception e) {
      String[] messages = { "Failed to Post Request", e.toString() };
      ErrorInfoWarningDialog.showErrorDialog(messages);
      return null;
    }
    String field = null;
    String value = "initial";
    for (int i = 0; ((!"Set-Cookie".equals(field)) && (value != null)); i++) {
      field = urlConn.getHeaderFieldKey(i);
      value = urlConn.getHeaderField(i);
    }
    if ("Set-Cookie".equals(field)) {
      int separatorLoc = value.indexOf("=");
      _cookieName = value.substring(0, separatorLoc);
      _cookieValue = value.substring((separatorLoc + 1), value.indexOf(";"));
      System.out.println("CookieName: " + _cookieName + " CookieValue: " + _cookieValue);
    }
    StringBuffer result = new StringBuffer();
    try {
      BufferedReader in = new BufferedReader(new InputStreamReader(urlConn.getInputStream()));
       String line;
       while(null != ((line = in.readLine()))) {
         result.append(line);
       }
       in.close();            
    } 
    catch (Exception e) {
      String[] messages = { "HTTPCommModule::Failed to Read Reply", e.toString() };
      ErrorInfoWarningDialog.showErrorDialog(messages);
      return null;
    }
    startTimer();
    return result.toString();
  }  
  
  /**
   * Starts the timer which is used by this comm module to track
   * the time elapsed between posting of a request to the server and
   * the receipt of the reply. The timer pops up a dialog
   * after every 30 minutes. The application terminates after 180 minutes
   * of inactivity.
   * 
   * @see com.conoco.cfe.client.messaging.HTTPCommModule#TimerAction
   */
  private void startTimer() {
     if (_timer == null) {
      _timer = new Timer(ClientConstants.DEFAULT_TIMEOUT_INTERVAL, new TimerAction());
      _timer.setRepeats(true);
    }
    _timer.start();
  }
  
  /**
   * Stops the timer which is used by this comm module to track
   * the time elapsed between posting of a request to the server and
   * the receipt of the reply. The timer pops up a dialog
   * after every 30 minutes. The application terminates after 180 minutes
   * of inactivity.
   */
  private void stopTimer() {
    if (_timer != null) {
      _timer.stop();
    }
  }
  
  /**
   * Inner class to implement the action that is executed by the
   * timer used by this comm module to track inactivity.
   */
  class TimerAction implements ActionListener {
    private int timeout_count = 0;
    public void actionPerformed(ActionEvent e) {
      timeout_count++;
      if (Client.RESET_TIMEOUT){
        timeout_count = 1;
      }
      if (timeout_count == 1) {
        Client.RESET_TIMEOUT = false;
      }
      if (timeout_count == 4) {
        String [] messages = { "Warning.  Session has been inactive for 120 minutes" };
        ErrorInfoWarningDialog.showNonmodalWarningDialog(messages);
      }
      else if (timeout_count == 5) {
        String [] messages = { "Warning.  Session will be terminated if no activity in the next 30 minutes" };
        ErrorInfoWarningDialog.showNonmodalWarningDialog(messages);
      }
      else if (timeout_count >= 6) {
        String [] messages = { "Terminating Application due to inactivity" };
        
        ErrorInfoWarningDialog.showWarningDialog(messages);
        sendMessage("<?xml version=\"1.0\" ?><Message><Request><TerminateApp /></Request></Message>");
        System.exit(0);
      }        
    }
  }
}
