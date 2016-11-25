// ClientConstants.java

package com.conoco.cfe.client;

import java.awt.Font;
import java.awt.Color;

/**
 * A general-purpose class to declare some client-side constants.
 */
public abstract class ClientConstants {
    
  /**
   * Variable for available fonts.
   * 
   * @serial
   */
  protected static String[] _fonts;  
      
  /**
   * The default server name
   * 
   * @serial
   */
  public static final String DEFAULT_SERVER_NAME = "poeplc02.po.conoco.com";
    
  /**
   * Constant to identify an undefined window id
   * 
   * @serial
   */
  public static final int UNDEFINED_WINDOW_ID = -99;
  
  /**
   * Default user name
   * 
   * @serial
   */
  public static final String DEFAULT_LOGIN_SERVLET_NAME = 
    "http://poeplc02.po.conoco.com/servlets/com.conoco.cfe.servlet.LoginServlet";
  
  /**
   * Default script name
   * 
   * @serial
   */
  public static final String DEFAULT_SCRIPT_NAME = "runrmi_linux";
  
  /**
   * Default servlet name
   * 
   * @serial
   */
  public static final String DEFAULT_SERVLET_NAME = 
    "http://poeplc02.po.conoco.com/servlets/com.conoco.cfe.servlet.MessagePasser";
  
  /**
   * Default XML document path
   * 
   * @serial
   */
  public static final String DEFAULT_XML_DOC_PATH = "/Conoco/xml";   
  
  /**
   * Variable for storing the default cookie name
   * 
   * @serial
   */
  public static String COOKIE_NAME = null;
  
  /**
   * Variable for storing the default cookie value
   * 
   * @serial
   */
  public static String COOKIE_VALUE = null;
  
  /**
   * Default label font
   * 
   * @serial
   */
  private static  Font DEFAULT_LABEL_FONT;
  
  /**
   * Default field font
   * 
   * @serial
   */
  private static  Font DEFAULT_FIELD_FONT;
  
  /**
   * Default array font
   * 
   * @serial
   */
  private static  Font DEFAULT_ARRAY_FONT;
  
  /**
   * Default path for the backend libraries
   *
   * @serial
   */
  public static final String DEFAULT_BACKEND_LIB_PATH = null;   
  
  /**
   * Variable for the mouse help mode 
   * 
   * @serial
   */
  public static final String MOUSE_HELP_MODE = "mouse";
  
  /**
   * Variable used to setup inactivity timer
   * (set to be 30 minutes)
   */
  public static final int DEFAULT_TIMEOUT_INTERVAL = 30 * 60 * 1000;   

  /**
   * Variable used for the Client Version Number
   */
  protected static String CLIENT_VERSION = "1.0";

  /**
   * Variable for the focus help mode 
   * 
   * @serial
   */
  public static final String FOCUS_HELP_MODE = "focus";
   
   /**
   * Color defined for Hightlight 1
   * 
   * @serial
   */
  public static Color HLIGHT1 = new Color(0, 255, 0);

   /**
   * Color defined for Hightlight 2
   * 
   * @serial
   */
  public static Color HLIGHT2 = new Color(255, 0, 0);
  
  /**
   * Color defined for purple hightlight
   * 
   * @serial
   */
  public static Color HLIGHT3 = new Color(204, 204, 255);
  
  public static Font getDefaultLabelFont() {
    return DEFAULT_LABEL_FONT;
  }
  
  public static Font getDefaultFieldFont() {
    return DEFAULT_FIELD_FONT;
  }
  
  public static Font getDefaultArrayFont() {
    return DEFAULT_ARRAY_FONT;
  }
  
  public static String getClientVersion() {
    return CLIENT_VERSION;
  }
  
  public static Color getFirstHighlightColor() {
    return HLIGHT1;
  }
  
  public static Color getSecondHighlightColor() {
    return HLIGHT2;
  }

  public static Color getThirdHighlightColor() {
    return HLIGHT3;
  }

  public static void setDefaultLabelFont(Font f) {
    DEFAULT_LABEL_FONT = f;
  }
  
  public static void setDefaultFieldFont(Font f) {
    DEFAULT_FIELD_FONT = f;
  }
  
  public static void setDefaultArrayFont(Font f) {
    DEFAULT_ARRAY_FONT = f;
  }
  
  public static void setClientVersion(String s) {
    CLIENT_VERSION = s;
  }
  
  public static void setFirstHighlightColor(Color c) {
    HLIGHT1 = c;
  }
  
  public static void setSecondHighlightColor(Color c) {
    HLIGHT2 = c;
  }

  public static void setThirdHighlightColor(Color c) {
    HLIGHT3 = c;
  }
}
