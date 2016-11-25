// DirectConnectCommModule.java

package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.ClientConstants;
import com.conoco.cfe.client.Client;

import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;

import com.conoco.cfe.server.messaging.DirectMessageTransport;

import com.conoco.xml.XML;
import com.conoco.xml.TextNode;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.net.HttpURLConnection;

import java.util.Hashtable;

import javax.swing.JFrame;
import javax.swing.Timer;


/**
 * A comm module that bypasses the middle-tier to send
 * messages to the server.
 */
public class DirectConnectCommModule  implements CommModule {
  /**
   * Variable for the server-side message transporter
   * 
   * @serial
   */
  protected DirectMessageTransport _transport;

  /**
   * Inactivity timer
   *
   */
  protected Timer _timer;

  /**
   * Constructs a new comm module.
   */  
  public DirectConnectCommModule() {
    try {
      _transport = new DirectMessageTransport(181*60*1000);
    } 
    catch ( Exception en ) {
      en.printStackTrace();  
    }
  }  
  
  /**
   * Sends the message to the server.
   * 
   * @param message the client message to be sent to the server
   * @return the server response
   */
  public String sendMessage(String message) {
    try {
      stopTimer();
      String res = _transport.sendMessage(message);
      startTimer();
      return res;
    } 
    catch (Exception en) {
      return getErrorMessage(en.toString());
    }
  }
  
  /**
   * Returns the XML for an "Error" message.
   * 
   * @param message the message text
   * @return string that contains the corresponding XML
   */
  private String getErrorMessage(String message) {
    XML res = new XML("Reponse");
    XML error = new XML("Error");    
    error.addAttribute("elements", message);    
    error.addAttribute("windowId", "0");
    res.addElement(error);
    return res.toString();
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
      _timer = new Timer(ClientConstants.DEFAULT_TIMEOUT_INTERVAL, 
                          new TimerAction());
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
        String [] messages = { "\n\n\n\n Session has been inactive for 120 minutes" };
        ErrorInfoWarningDialog.showNonmodalWarningDialog(messages);
      }
      else if (timeout_count == 5) {
        String [] messages = { "\n\n\n\n Session will be terminated if no activity in the next 30 minutes" };
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
