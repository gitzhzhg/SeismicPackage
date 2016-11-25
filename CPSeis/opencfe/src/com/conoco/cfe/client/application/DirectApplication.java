///
/// DirectApplication.java
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  5. 04-06-2004 SMCook   Using new MiscellaneousParameter handler to send
///                          custom icps pathname to the Fortran at startup.
///  4. 09-05-2002 SMCook   Added FocusWatchingThread (currently disabled).
///                         Cleaned up line length and tabs.
///  3. 10-04-2001 SMCook   Changed in constructor super(prefs) to
///                          super(prefs, false) to properly notify back end
///                          that this is not client/server mode.
///

package com.conoco.cfe.client.application;

import com.conoco.shared.FocusWatchingThread;

import com.conoco.cfe.client.Client;
import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;
import com.conoco.cfe.client.gui.DirectLoginDialog;

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

import com.conoco.cfe.client.gui.controller.GUIController;
import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.gui.controls.WindowControl;
 
import java.awt.Window;
import java.awt.Color;

import java.net.URL;
import java.net.MalformedURLException;

/**
 * A client application that bypasses the middle tier. 
 * 
 * @see com.conoco.cfe.client.messaging.DirectConnectCommModule
 */
public class DirectApplication extends Application {

  /**
   * Constructs a new application.
   * 
   * @param prefsFileURL the preferences file URL string
   */
  public DirectApplication(String prefsFileURL) {  
    super(prefsFileURL, false);  

    //Thread t = new FocusWatchingThread(2000);
    //t.start();
  }
 
  /**
   * Reads the preferences from the preferences file. This is 
   * a protected method called by the contructor.
   * 
   * @param prefsFileURL the string describing the preferences file URL
   */
  protected void readPreferences(String prefsFileURL) {
    PreferencesReader prefsReader = new PreferencesReader(prefsFileURL);
  }

  /**
   * Starts the execution of the client by sending an
   * "InitializeApp" message to the server. This is a protected method 
   * called by the constructor.
   */
  protected void startApplication() {
    String sharedLibPath = Preferences.getBackEndLibPath();
    Console.logMessage(
      "Client Version number is: " +ClientConstants.getClientVersion());    
    _commController.transmitMessage(ClientConstants.UNDEFINED_WINDOW_ID, 
      "BackEndLibPath", null , sharedLibPath);
    _commController.transmitMessage(ClientConstants.UNDEFINED_WINDOW_ID, 
      "InitializeApp", "DirectApplication" , null);
    _commController.transmitMessage(ClientConstants.UNDEFINED_WINDOW_ID, 
      "MiscellaneousParameter", "CUSTOMPATH" ,
      sharedLibPath.substring(0, sharedLibPath.lastIndexOf("/")));
  }
 
  /**
   * Initializes the client-side modules. This is a protected method
   * called by the constructor.
   */
  protected void initializeModules() {
    _commModule = new DirectConnectCommModule();
    _commController = new AppCommController();
    _encoder = new XMLMessageEncoder();
    _decoder = new XMLMessageDecoder();
    _guiController = new AppGUIController();
  }
 
  /**
   * Displays the login dialog that performs login validation.
   * This is a protected method called by the constructor.
   */
  protected void showLoginDialog() {
    /*DirectLoginDialog d = new DirectLoginDialog("Direct Login");
    d.setVisible(true);
    */
  }

  /**
   * Entry point for the JVM. The application requires the string 
   * describing the preferences file URL as its (only) argument.
   * 
   * @param args the command line arguments
   */
  public static void main(String[] args) {

    if ( args.length == 0 ) {

      System.err.println("Usage: java DirectApplication prefsFileURL");
      System.exit(1);

    }

    Client.init();

    new DirectApplication(args[0]);

  }

}
