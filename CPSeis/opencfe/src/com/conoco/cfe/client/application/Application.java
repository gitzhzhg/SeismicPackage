///
/// Application.java  (client-server)
///
///     Date       Author   Alterations
///----------------------------------------------------------------------------
///  8.
///  7. 10-25-2002 SMCook   Added HelpWindowControl.setup() call.
///  6. 03-28-2002 SMCook   Changed occurrences of poepsn0?.po.conoco.com to
///                          poepsn0?.conoco.net.
///  5. 10-08-2001 SMCook   Added static isClientServer() function to allow
///                          easy query from other classes.
///  4. 10-04-2001 SMCook   Added isClientServer variable to more clearly
///                          control whether "DirectApplication" or
///                          "CSApplication" is sent to the back end cfe (f90)
///                          code.  "CSApplication" was erroneously hardwired
///                          during the recent transition to a single code base
///                          (merging the direct and client/server code trees).
///  3. 09-24-2001 SMCook   Changes aimed at getting cfecustom to run remotely.
///

package com.conoco.cfe.client.application;

import com.conoco.shared.Logger;

import com.conoco.cfe.client.Client;
import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Console;

import com.conoco.cfe.client.gui.ErrorInfoWarningDialog;
import com.conoco.cfe.client.gui.LoginDialog;

import com.conoco.cfe.client.gui.controls.HelpWindowControl;
import com.conoco.cfe.client.gui.controls.WindowControl;

import com.conoco.cfe.client.gui.controller.GUIController;
import com.conoco.cfe.client.gui.controller.AppGUIController;

import com.conoco.cfe.client.messaging.PreferencesReader;
import com.conoco.cfe.client.messaging.Preferences;
import com.conoco.cfe.client.messaging.HTTPCommModule;
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

/**
 * The primary client application. The client requires the URL
 * of the preferences file as its argument.
 */
public class Application {
  
  /**
   * Variable for the string that represents the 
   * prefs file URL
   * 
   * @serial
   */
  protected String _prefsFileURLString;
  
  /**
   * Variable holds true or false depending on startup mode.
   */
  private static boolean _isClientServer;

  /**
   * Variable for the string that represents the 
   * servlet URL
   * 
   * @serial
   */
  protected String _servletURLString;
  
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
   * Variable holding username.  Added by SMCook to allow
   * for constructor that is compatible with cfecustom.
   */
  private String _username;
 
  /**
   * Constructs a new client-server application.
   * 
   * @param prefsFileURL -- the preferences file URL string
   */

//
// cfecustom case: URL is automatically generated from remote username
//                  read a dummy known working prefs file
//                  do showLoginDialog() to obtain username
//                  read the user's prefs file
//    normal case: URL is supplied directly as argument
//                  showLoginDialog() can be later
//

  public Application(String prefsFileURL, boolean isClientServer) {

    _isClientServer = isClientServer;

    if(prefsFileURL.indexOf("custom") >= 0) {

      int platform=Logger.getPlatformCode();
      String customPrefs = "";

      switch(platform) {
        case Logger.PLATFORM_WINDOWS:
          customPrefs = "prefs.xml.windows";
        break;

        case Logger.PLATFORM_LINUX:
          customPrefs = "prefs.xml.linux";
        break;

        case Logger.PLATFORM_SOLARIS:
          customPrefs = "prefs.xml.sol";
        break;

        default:
          System.err.println("Prefs file not available for your platform.");
          System.exit(0);
      }

//
// _username is set in showLoginDialog()
//   (cfecustom needs this information to find the correct prefs file)
//

      readPreferences(    "http://poepsn03.conoco.net/Conoco/xml/prefs.xml.windows.client");
      showLoginDialog();
      _prefsFileURLString="http://poepsn03.conoco.net/Conoco/xml/CFEtest/" + _username + "/" + customPrefs;
      System.err.println("Preferences file is '" + _prefsFileURLString + "'");
      readPreferences(_prefsFileURLString);
    }
    else {
      _prefsFileURLString=prefsFileURL;
      System.err.println("Preferences file is '" + _prefsFileURLString + "'");
      readPreferences(_prefsFileURLString);
      showLoginDialog();
    }

    initializeModules();
    glueModules();
    startApplication();

    HelpWindowControl.setup();
  }
 
  /**
   * Displays the login dialog that performs login validation.
   * This is a protected method called by the constructor.
   */
  protected void showLoginDialog() {
//    LoginDialog d = new LoginDialog("Login: " + Preferences.getServerName());
    LoginDialog d = new LoginDialog("Login");
    d.setVisible(true);
    _username=d.username;
  }
  
  /**
   * Reads the preferences from the preferences file. This is 
   * a protected method called by the contructor.
   * 
   * @param prefsFileURL the string describing the preferences file URL
   */
  protected void readPreferences(String prefsFileURL) {
    try {
      PreferencesReader prefsReader = new PreferencesReader(prefsFileURL);
      _servletURLString = Preferences.getServletName();
    } catch (Exception e) {
      System.exit(1);     //SMCook added - program used to proceed merrily along
    }
  }
  
  /**
   * Initializes the client-side modules. This is a protected method
   * called by the constructor.
   */
  protected void initializeModules() {
    URL servletURL = null;
    
    try {
      servletURL = new URL(_servletURLString);
    } 
    catch ( MalformedURLException en) {
      String[] m = {"Application: Malformed servlet URL"};
      ErrorInfoWarningDialog.showErrorDialog(m);
    }
    
    _commModule = new HTTPCommModule(servletURL);
    _commController = new AppCommController();
    _encoder = new XMLMessageEncoder();
    _decoder = new XMLMessageDecoder();
    _guiController = new AppGUIController();
  }
  
  /**
   * Joins the initialized modules together to create the 
   * client-side machinery. This is a protected method 
   * called by the constructor.
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
   * Allow other classes to inquire about client/server mode.
   */
  public static final boolean isClientServer() {
    return _isClientServer;
  }

  /**
   * Starts the execution of the client by sending an
   * "InitializeApp" message to the server. This is a protected method 
   * called by the constructor.
   */
  protected void startApplication() {

    Console.logMessage( "Client Version number is: " + ClientConstants.getClientVersion());    

    _commController.transmitMessage(ClientConstants.UNDEFINED_WINDOW_ID, 
      "BackEndLibPath", null , Preferences.getBackEndLibPath());

    if(_isClientServer)
      _commController.transmitMessage(
        ClientConstants.UNDEFINED_WINDOW_ID, "InitializeApp", "CSApplication", null);
    else
      _commController.transmitMessage(
        ClientConstants.UNDEFINED_WINDOW_ID, "InitializeApp", "DirectApplication", null);
  }
  
  /**
   * Entry point for the JVM. The application requires the string 
   * describing the preferences file URL as its (only) argument.
   * 
   * @param args the command line arguments
   */
  public static void main(String[] args) {

    if ( args.length == 0 ) {
      System.out.println();
      System.out.println("USAGE depends on whether user desires to run cfecustom or not.  If the argument");
      System.out.println("does not contain the string 'prefs.', the 'cfecustom case' will run.");
      System.out.println(
        "  Example 1 (cfecustom):  java Application custom");
      System.out.println(
        "  Example 2 (cfecustom):  java Application http://poepsn03.conoco.net/Conoco/xml/CFEtest");
      System.out.println(
        "  Example 3            :  java Application http://poepsn03.conoco.net/Conoco/xml/prefs.xml.windows.client");
      System.out.println();

      System.exit(1);
    }

    Client.init();
    new Application(args[0], true);
  }

}
