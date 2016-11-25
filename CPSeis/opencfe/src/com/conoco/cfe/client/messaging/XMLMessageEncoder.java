// XMLMessageEncoder.java
 
package com.conoco.cfe.client.messaging;

import com.conoco.cfe.client.ClientConstants;

import com.conoco.cfe.client.application.Console;

import com.conoco.xml.XML;
import com.conoco.xml.TextNode;

import java.net.URLEncoder;

import java.util.Hashtable;
import java.util.Enumeration;

/**
 * This class is used by the client's state machine to send XML
 * messages to be processed by the server.
 */
public class XMLMessageEncoder implements MessageEncoder {
  /**
   * Declares an XML node for the request made by the client.
   * 
   * @serial
   */    
  XML _request;
    
  /**
   * Constructs a new message writer. This is an empty constructor.
   */    
  public XMLMessageEncoder() {
    _request = new XML("Request");  
  }  
 
  /**
   * Sends a request to the server for setting a value.
   *
   * @param windowId the id of the window
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param value The value for the action.  This field is ignored for some 
   *        action types.
   */
  public void setValue(int windowId, String action, String keyword, String value) {
    _request = new XML("Request");
    XML xmlAction = new XML(action);
    if ( windowId != ClientConstants.UNDEFINED_WINDOW_ID ) {
      xmlAction.addAttribute("windowId", String.valueOf(windowId));
    }
    if ( keyword != null) {
      xmlAction.addAttribute("keyword", keyword);
    } 
    if ( value != null ) {
      if ( action.equals("ItemsSelected") ) {
        xmlAction.addAttribute("elements", URLEncoder.encode(value));
      } 
      else {
        xmlAction.addAttribute("value", URLEncoder.encode(value));
      }
    }
    _request.addElement(xmlAction);
    Console.logMessage(xmlAction.toString());
  }

  /**
   * Sends a request to the server for setting an array of values.
   *
   * @param windowId the id of the window
   * @param action A unique string inidicating the action type.
   * @param keyword A keyword identifying the object upon which the action is
   *        taken.  This field is ignored for some action types.
   * @param value the changed value 
   * @param index index of the object changed
   */
  public void setArray(int windowId, String action, String keyword, String value, int start, int end) {
    _request = new XML("Request");
    XML xmlAction1 = new XML(action);
    if ( windowId != ClientConstants.UNDEFINED_WINDOW_ID ) {
      xmlAction1.addAttribute("windowId", String.valueOf(windowId));
    }
    if ( keyword != null) {
      xmlAction1.addAttribute("keyword", keyword);
    } 
    if ( value != null ) {
      if ( action.equals("ModifyArrayElement") ) {
        xmlAction1.addAttribute("value", URLEncoder.encode(value)); 
      } 
      else {
        xmlAction1.addAttribute("elements", URLEncoder.encode(value));  
      }
    }
    if ( start != -1 ) {
      xmlAction1.addAttribute("startElement", String.valueOf(start+1));
    }
    if ( end != -1) {
      xmlAction1.addAttribute("endElement", String.valueOf(end+1));
    }    
    Console.logMessage(xmlAction1.toString());
    _request.addElement(xmlAction1);
  }
      
  /**
   * Build the XML header for the return message.
   *
   * @param documentType A character string repersenting the XML element that acts
   * as the root of the XML message.
   * @return The first two lines in an XML message (XML version #, document type,
   * and DTD location).
   * @return the XML document header
   */
  protected String buildHeader(String documentType) {
    return XMLInfo.XML_VERSION;
  }
    
  /**
   * Starts writing the message. The message is written as a string 
   * which can be retreived once writing is complete. This method should
   * be called before the message parameters have been set.
   * 
   * @return the XML document as a string
   */
  public String encode() {  
    XML message = new XML("Message");
    message.addElement(_request);    
    return buildHeader("Message") + message.toString();  
  }
}