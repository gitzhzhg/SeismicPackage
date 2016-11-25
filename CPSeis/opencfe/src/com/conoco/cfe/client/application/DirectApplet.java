// DirectApplet.java

package com.conoco.cfe.client.application;

import com.conoco.cfe.client.Client;
import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;

import com.conoco.cfe.client.gui.controller.GUIController;
import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.WindowControl;

import com.conoco.cfe.client.messaging.PreferencesReader;
import com.conoco.cfe.client.messaging.Preferences;
import com.conoco.cfe.client.messaging.DirectConnectCommModule;
import com.conoco.cfe.client.messaging.CommModule;
import com.conoco.cfe.client.messaging.CommController;
import com.conoco.cfe.client.messaging.AppCommController;
import com.conoco.cfe.client.messaging.MessageEncoder;
import com.conoco.cfe.client.messaging.MessageDecoder;
import com.conoco.cfe.client.messaging.XMLMessageDecoder;
import com.conoco.cfe.client.messaging.XMLMessageEncoder;

import java.awt.Window;
import java.net.URL;
import java.net.MalformedURLException;

import javax.swing.JApplet;

/**
 * Run the application as an applet
 */
public class DirectApplet extends JApplet {
    
  /**
   * Variable for the comm module
   * 
   * @serial
   */
  protected CommModule _commModule;
  
  /**
   * Variable for the comms controller
   * 
   * @serial
   */
  protected CommController _commController;
  
  /**
   * Variable for the message decoder
   * 
   * @serial
   */
  protected MessageDecoder _decoder;
  
  /**
   * Variable for the message encoder
   * 
   * @serial
   */
  protected MessageEncoder _encoder;
  
  /**
   * Variable for the GUI controller
   * 
   * @serial
   */
  protected GUIController _guiController;
  
  /**
   * Constructs a new application.
   */
  public DirectApplet() {  
  }
  
  /**
   * Initialize the application
   * 
   * @param prefsFileURL the preferences file URL string
   */
  public void init() {
    String prefsFileURL = getParameter("prefs");
    
    URL docBase = getDocumentBase();

    if ( prefsFileURL != null ) {
      readPreferences("http://" + docBase.getHost() + prefsFileURL);
      initializeModules();
      glueModules();
    } 
    else {
      System.err.println("Preferences file is null");
    }            

    Client.init();
  }
  
  public void start() {
    startApplication();
  }
  
  /**
   * Reads the preferences from the preferences file. 
   * 
   * @param prefsFileURL the string describing the preferences file URL
   */
  protected void readPreferences(String prefsFileURL) {
    PreferencesReader prefsReader = new PreferencesReader(prefsFileURL);
  }
  
  /**
   * Initializes the client-side modules. 
   */
  protected void initializeModules() {
    _commModule = new DirectConnectCommModule();
    _commController = new AppCommController();
    _encoder = new XMLMessageEncoder();
    _decoder = new XMLMessageDecoder();
    _guiController = new AppGUIController();
  }
  
  /**
   * Joins the initialized modules together to create the 
   * client-side machinery. 
   */
  protected void glueModules() {
    _guiController.setCommController(_commController);
    _commController.setMessageEncoder(_encoder);
    _commController.setMessageDecoder(_decoder);
    _commController.setCommModule(_commModule);
    _commController.setReplyActionHandlers( 
    _guiController.getReplyActionHandlers() );
  }
  
  /**
   * Starts the execution of the client by sending an
   * "InitializeApp" message to the server.
   */
  protected void startApplication() {
    _commController.transmitMessage(ClientConstants.UNDEFINED_WINDOW_ID,
      "InitializeApp", null , null);
  }
}