// Client.java

package com.conoco.cfe.client;

import com.conoco.cfe.client.gui.controls.ConocoFocusManager;
import com.conoco.cfe.utils.FontLoader;

import java.awt.Font;

import javax.swing.FocusManager;
import javax.swing.DefaultFocusManager;


/**
 * Performs client configuration tasks.
 */
public class Client {
  
  /**
  * A flag that is set to true when a modal error-info-warning
  * message is shown and is set to false when the dialog is
  * dismissed. This flag is used by the GUI controls to test
  * whether a error-info-warning is going to be displayed or not.
  * If it is, then the control will not sent a focus lost 
  * notification to the server.
  * 
  * @see com.conoco.cfe.client.gui.ErrorInfoWarningDialog
  * @serial
  */
  public static boolean FOCUS_FLAG;
   
  /**
  * A flag that is set to true when user reset the time out.
  * 
  * @see com.conoco.cfe.client.gui.ErrorInfoWarningDialog
  * @serial
  */
  public static boolean RESET_TIMEOUT;
    
  /**
   * Performs initialization tasks. Specifically, it sets the 
   * custom focus manager and loads the available fonts.
   */
  public static void init() {  
    FocusManager.setCurrentManager(new ConocoFocusManager());
    getFonts();
  }
  
  /**
   * Returns the array of fonts available on the system.
   *
   * @return fonts the available fonts
   */
  protected static void getFonts() {
    // need to call this to get around a bug in java
    FontLoader.loadAllFonts();
    
    // initialize the default fonts
    ClientConstants.setDefaultLabelFont(new Font("Courier", Font.PLAIN, 12));
    ClientConstants.setDefaultFieldFont(new Font("Courier", Font.BOLD, 12));
    ClientConstants.setDefaultArrayFont(new Font("Courier", Font.PLAIN, 12));        
  }  
}